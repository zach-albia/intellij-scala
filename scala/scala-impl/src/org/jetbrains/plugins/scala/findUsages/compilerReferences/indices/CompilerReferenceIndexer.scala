package org.jetbrains.plugins.scala.findUsages.compilerReferences
package indices

import java.io.File
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.progress.{ProgressIndicator, Task}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Disposer
import com.intellij.util.containers.ContainerUtil
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.findUsages.compilerReferences.bytecode.{ClassfileParser, CompiledScalaFile}
import org.jetbrains.plugins.scala.findUsages.compilerReferences.indices.IndexerFailure._
import org.jetbrains.plugins.scala.findUsages.compilerReferences.indices.IndexingStage._
import org.jetbrains.plugins.scala.indices.protocol.{CompilationInfo, CompiledClass}

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

private class CompilerReferenceIndexer(project: Project, expectedIndexVersion: Int) {
  import CompilerReferenceIndexer._

  private[this] val indexerJobQueue = new ConcurrentLinkedQueue[IndexerJob]()
  private[this] val nThreads        = Runtime.getRuntime.availableProcessors()

  private[this] var indexingExecutor: ExecutorService                 = _
  private[this] var indexWriter: Option[ScalaCompilerReferenceWriter] = None

  private[this] val jobFailures  = ContainerUtil.newConcurrentSet[IndexerJobFailure]()
  private[this] val fatalFailure = new AtomicReference[Option[Throwable]](Option.empty)

  Disposer.register(project, () => shutdown())

  private[this] def shutdown(): Unit =
    if (!isShutdown) indexingExecutor.shutdownNow()

  private[this] def onException(e: Throwable, shouldClearIndex: Boolean): Unit = {
    fatalFailure.updateAndGet(_.orElse(Option(e)))
    if (shouldClearIndex) indexWriter.foreach(_.close(shouldClearIndex = true))
    shutdown()
  }

  private[this] def isShutdown: Boolean =
    indexingExecutor == null || indexingExecutor.isShutdown

  private[this] def checkInterruptStatus(): Unit =
    if (Thread.interrupted()) throw new InterruptedException

  private def parseClassfiles(writer: ScalaCompilerReferenceWriter): Unit =
    try {
      while (!indexerJobQueue.isEmpty) {
        checkInterruptStatus()
        val job = indexerJobQueue.poll()

        try job match {
          case ProcessRemovedSource(file) => writer.processDeletedFile(file.getPath)
          case ProcessGeneratedClasses(classes) =>
            val sourceFile = classes.head.source // guaranteed to be non-empty
            val classfiles = classes.map(_.output)
            val parsed     = ClassfileParser.parse(classfiles)
            val data       = CompiledScalaFile(sourceFile, parsed, writer)
            writer.registerClassfileData(data)
          case null => ()
        } catch { case NonFatal(e) => jobFailures.add(IndexerJobFailure(job, e)) }
      }
    } catch { case e: Throwable => onException(e, shouldClearIndex = false) }

  private[this] def initialiseExecutorIfNeeded(): Unit =
    if (isShutdown) indexingExecutor = Executors.newFixedThreadPool(nThreads)

  def toTask(job: IndexingStage): Task.Backgroundable =
    job match {
      case OpenWriter(isCleanBuild) => task(project, "Initializing compiler indices writer") { _ =>
        initialiseExecutorIfNeeded()
        indexWriter = indexDir(project).flatMap(ScalaCompilerReferenceWriter(_, expectedIndexVersion, isCleanBuild))
      }
      case CloseWriter(onFinish) => task(project, "Closing compiler indices writer") { _ =>
        val maybeFatalFailure = fatalFailure.get().map(FatalFailure)

        val maybeFailure = maybeFatalFailure.orElse {
          if (!jobFailures.isEmpty) FailedToParse(jobFailures.asScala).toOption
          else                        None
        }

        cleanUp(maybeFatalFailure.isDefined)
        onFinish(maybeFailure)
      }
      case ProcessCompilationInfo(info, onFinish) => new IndexCompilationInfoTask(info, onFinish)
      case InvalidateIndex(index) =>
        task(project, "Invalidating compiler indices") { _ =>
          index.foreach(_.close())
          cleanUp(shouldClearIndex = true)
        }
    }

  private[this] def cleanUp(shouldClearIndex: Boolean): Unit =
    try indexWriter match {
      case Some(writer)             => writer.close(shouldClearIndex)
      case None if shouldClearIndex => removeIndexFiles(project)
      case _                        => ()
    } finally {
      indexWriter = None
      jobFailures.clear()
      fatalFailure.set(Option.empty)
    }

  private final class IndexCompilationInfoTask(info: CompilationInfo, callback: () => Unit)
      extends Task.Backgroundable(project, ScalaBundle.message("bytecode.indices.indexing"), true) {

    private[this] def processInfo(progressIndicator: ProgressIndicator): Unit = {
      val start = System.currentTimeMillis()
      indexWriter match {
        case None =>
          log.warn("Failed to index compilation info due to index writer being disposed.")
          callback()
        case Some(writer) =>
          try {
            info.removedSources.iterator.map(ProcessRemovedSource).foreach(indexerJobQueue.add)

            info.generatedClasses.groupBy(_.source).foreach {
              case (_, classes) => indexerJobQueue.add(ProcessGeneratedClasses(classes))
            }

            val tasks = (1 to nThreads).map(_ => toCallable(parseClassfiles(writer)))

            indexingExecutor.invokeAll(tasks.asJavaCollection)
          } catch {
            case e: Throwable => onException(e, shouldClearIndex = true)
          } finally callback()
      }

      indexerJobQueue.clear()
      val delta = System.currentTimeMillis() - start
      log.debug(s"Reindexed ${info.generatedClasses.size} classes and " +
        s"${info.removedSources.size} removed sources in $delta millis")
    }

    override def run(progressIndicator: ProgressIndicator): Unit =
      if (!isShutdown) {
        if (!info.isEmpty) processInfo(progressIndicator)
        else               callback()
      } else log.error("Unable to start indexing, since executors are shutdown.")
  }
}

private[compilerReferences] object CompilerReferenceIndexer {
  private val log = Logger.getInstance(classOf[CompilerReferenceIndexer])

  private[compilerReferences] final case class IndexerJobFailure(job: IndexerJob, cause: Throwable) {
    def errorMessage: String = s"Indexer job $job failed."

    def classfiles: Set[File] = job match {
      case ProcessRemovedSource(_)       => Set.empty
      case ProcessGeneratedClasses(data) => data.map(_.output)
    }
  }

  private[compilerReferences] sealed trait IndexerJob
  private[compilerReferences] final case class ProcessRemovedSource(file:    File)               extends IndexerJob
  private[compilerReferences] final case class ProcessGeneratedClasses(data: Set[CompiledClass]) extends IndexerJob
}
