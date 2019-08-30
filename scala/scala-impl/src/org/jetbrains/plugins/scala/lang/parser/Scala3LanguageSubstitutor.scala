package org.jetbrains.plugins.scala
package lang
package parser

import com.intellij.lang.Language
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.LanguageSubstitutor

final class Scala3LanguageSubstitutor extends LanguageSubstitutor {

  override def getLanguage(virtualFile: VirtualFile, project: Project): Language = {
    val scalaExtension = ScalaFileType.INSTANCE.getDefaultExtension
    val parent = virtualFile.getParent

    virtualFile.getExtension match {
      case `scalaExtension` if parent != null =>
        val bytes = parent
          .getParent
          .getParent
          .getParent
          .findChild("build.sbt")
          .contentsToByteArray

        if (new String(bytes).contains("0.17.0"))
          Scala3Language.INSTANCE
        else
          ScalaLanguage.INSTANCE
      case _ => null
    }
  }
}
