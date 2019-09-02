package org.jetbrains.plugins.scala
package lang
package parser

import com.intellij.lang.Language
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.LanguageSubstitutor
import org.jetbrains.plugins.scala.extensions.{NullSafe, ObjectExt}
import org.jetbrains.sbt.RichOption

final class Scala3LanguageSubstitutor extends LanguageSubstitutor {

  override def getLanguage(virtualFile: VirtualFile, project: Project): Language = {
    val scalaExtension = ScalaFileType.INSTANCE.getDefaultExtension
    val parent = virtualFile.getParent

    virtualFile.getExtension match {
      case `scalaExtension` if parent != null =>
        val projectRoot = parent
          .getParent.toOption
          .safeMap(_.getParent)
          .safeMap(_.getParent)
        val buildSbtContent = projectRoot
          .safeMap(_.findChild("build.sbt"))
          .safeMap(file => new String(file.contentsToByteArray))

        buildSbtContent match {
          case Some(contents) if contents.contains("0.17.") || contents.contains("0.18.") =>
            Scala3Language.INSTANCE
          case None =>
            ScalaLanguage.INSTANCE
        }
      case _ => null
    }
  }
}
