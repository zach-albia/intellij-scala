package org.jetbrains.plugins.scala
package projectHighlighting

import java.io.File

import com.intellij.openapi.module.Module
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.base.libraryLoaders.ScalaSDKLoader
import org.jetbrains.plugins.scala.util.TestUtils
import org.jetbrains.plugins.scala.util.reporter.ProgressReporter

class ScalacTestdataHighlightingTest_2_13
  extends ScalaLightCodeInsightFixtureTestAdapter with SeveralFilesHighlightingTest  {

  override protected def supportedIn(version: ScalaVersion): Boolean = version == Scala_2_13

  override def getProject = super.getProject

  override def getModule: Module = super.getModule

  override def librariesLoaders = Seq(
    ScalaSDKLoader(includeScalaReflect = true)
  )

  override val reporter = ProgressReporter.newInstance(getClass.getSimpleName, filesWithProblems = Map.empty, reportStatus = false)

  override def filesToHighlight: Array[File] = {
    val testDataPath = TestUtils.getTestDataPath + "/scalacTests/2.13/pos/"

    val dir = new File(testDataPath)
    dir.listFiles()
  }


  def testScalacTests(): Unit = doTest()
}