package org.jetbrains.plugins.scala.console.configuration

import com.intellij.execution.configurations.{ConfigurationFactory, ConfigurationType}
import com.intellij.openapi.project.DumbAware
import javax.swing.Icon
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.icons.Icons

class ScalaConsoleConfigurationType extends ConfigurationType with DumbAware {
  private val confFactory = new ScalaConsoleRunConfigurationFactory(this)

  override def getIcon: Icon = Icons.SCALA_CONSOLE

  override def getDisplayName: String = ScalaBundle.message("scala.console.config.display.name")

  override def getConfigurationTypeDescription: String = ScalaBundle.message("scala.console.config.scala.repl.run.configurations")

  override def getConfigurationFactories: Array[ConfigurationFactory] = Array[ConfigurationFactory](confFactory)

  override def getId: String = "ScalaScriptConsoleRunConfiguration"
}