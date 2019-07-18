package org.jetbrains.sbt.shell

import java.util.concurrent.atomic.AtomicReference

import com.intellij.openapi.application.ApplicationManager

import scala.concurrent.{Future, Promise}
import scala.util.Try

object ShellUIUtil {

  def inUIasync[T](f: =>T): Future[T] = {
    val p = Promise[T]()
    ApplicationManager.getApplication.invokeLater(() => p.complete(Try(f)))
    p.future
  }

  def inUIsync[T](f: =>T): T = {
    val p = new AtomicReference[T]
    ApplicationManager.getApplication.invokeAndWait(() => p.set(f))
    p.get()
  }
}
