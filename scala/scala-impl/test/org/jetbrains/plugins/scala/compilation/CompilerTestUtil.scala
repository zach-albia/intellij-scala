package org.jetbrains.plugins.scala.compilation

import com.intellij.openapi.application.ex.ApplicationManagerEx
import com.intellij.openapi.projectRoots.Sdk
import com.intellij.openapi.util.registry.Registry
import org.jetbrains.plugins.scala.compiler.ScalaCompileServerSettings

/**
  * @author Nikolay.Tropin
  */
object CompilerTestUtil {

  def enableCompileServer(enable: Boolean): Unit = {
    val compileServerSettings = ScalaCompileServerSettings.getInstance()
    assert(compileServerSettings != null, "could not get instance of compileServerSettings. Was plugin artifact built before running test?")
    compileServerSettings.COMPILE_SERVER_ENABLED = enable
    compileServerSettings.COMPILE_SERVER_SHUTDOWN_IDLE = true
    compileServerSettings.COMPILE_SERVER_SHUTDOWN_DELAY = 30
    val applicationEx = ApplicationManagerEx.getApplicationEx
    applicationEx.setSaveAllowed(true)
    applicationEx.saveSettings()
  }

  def forceLanguageLevelForBuildProcess(jdk: Sdk): Unit =
    jdk.getHomeDirectory match {
      case null =>
        throw new RuntimeException(s"Failed to set up JDK, got: $jdk")
      case homeDirectory =>
        val jdkHome = homeDirectory.getCanonicalPath
        Registry.get("compiler.process.jdk").setValue(jdkHome)
    }

  def setCompileServerJdk(getTestProjectJdk: Sdk): Unit = {
    val compilerServerSettings = ScalaCompileServerSettings.getInstance
    compilerServerSettings.USE_DEFAULT_SDK = false
    compilerServerSettings.COMPILE_SERVER_SDK = getTestProjectJdk.getName
  }
}
