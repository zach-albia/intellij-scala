package org.jetbrains.plugins.scala.testingSupport.locationProvider

import java.{util => ju}

import com.intellij.execution.testframework.sm.runner.SMTestLocator
import com.intellij.execution.{Location, PsiLocation}
import com.intellij.openapi.editor.Document
import com.intellij.openapi.project.Project
import com.intellij.psi._
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.scala.caches.ScalaShortNamesCacheManager
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ElementScope
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager

/**
 * User: Alexander Podkhalyuzin
 * Date: 24.02.2009
 *
 * For Specs, Specs2 and ScalaTest
 */
class ScalaTestLocationProvider extends SMTestLocator {
  private val SpecsHintPattern = """(\S+)\?filelocation=(.+):(.+)""".r

  private val ScalaTestTopOfClassPattern  = """TopOfClass:(\S+)TestName:(.+)""".r
  private val ScalaTestTopOfMethodPattern = """TopOfMethod:(\S+):(\S+)TestName:(.+)""".r
  private val ScalaTestLineInFinePattern  = """LineInFile:(\S+):(.+):(.+)TestName:(.+)""".r

  override def getLocation(protocolId: String, locationData: String, project: Project, scope: GlobalSearchScope): ju.List[Location[_ <: PsiElement]] = {
    protocolId match {
      case "scala" =>
        locationData match {
          case SpecsHintPattern(className, fileName, lineNumber) =>
            val classes = ScalaShortNamesCacheManager.getInstance(project).getClassesByFQName(className, scope)
            val found = classes.find(c => Option(c.getContainingFile).exists(_.name == fileName))

            found match {
              case Some(file) =>
                val res = new ju.ArrayList[Location[_ <: PsiElement]]()
                res.add(createLocationFor(project, file.getContainingFile, lineNumber.toInt))
                res
              case _ => searchForClassByUnqualifiedName(project, className)
            }
          case _ => searchForClassByUnqualifiedName(project, locationData)
        }
      case "scalatest" =>
        val res = new ju.ArrayList[Location[_ <: PsiElement]]()
        locationData match {
          case ScalaTestTopOfClassPattern(classFqn, testName) =>
            val classes = ScalaShortNamesCacheManager.getInstance(project).getClassesByFQName(classFqn, scope)
            val clazz = classes.find(!_.isInstanceOf[ScObject]).orElse(classes.headOption)
            clazz.foreach(c => res.add(PsiLocationWithName(project, c, testName)))
          case ScalaTestTopOfMethodPattern(classFqn, methodName, testName) =>
            val classes = ScalaShortNamesCacheManager.getInstance(project).
              getClassesByFQName(classFqn, GlobalSearchScope.allScope(project))
            val methodOwner = classes.find(!_.isInstanceOf[ScObject]).orElse(classes.headOption)
            methodOwner match {
              case Some(td: ScTypeDefinition) =>
                td.methodsByName(methodName).foreach { signature =>
                    res.add(PsiLocationWithName(project, signature.method, testName))
                }
              case _ =>
            }
            if (res.isEmpty && methodOwner.isDefined) {
              val methods = methodOwner.get.findMethodsByName(methodName, false)
              methods.foreach {
                method => res.add(PsiLocationWithName(project, method, testName))
              }
            }
          case ScalaTestLineInFinePattern(classFqn, fileName, lineNumber, testName) =>
            val classes = ScalaPsiManager.instance(project).getCachedClass(GlobalSearchScope.allScope(project), classFqn)
            val found = classes.find(c => c.containingFile.exists(_.name == fileName))
            found match {
              case Some(file) =>
                res.add(createLocationFor(project, file.getContainingFile, lineNumber.toInt, Some(testName)))
              case _ =>
            }
          case _ =>
        }
        res
      case _ => new ju.ArrayList[Location[_ <: PsiElement]]()
    }
  }

  private def searchForClassByUnqualifiedName(project: Project, locationData: String): ju.ArrayList[Location[_ <: PsiElement]] = {
    val res = new ju.ArrayList[Location[_ <: PsiElement]]()
    ElementScope(project).getCachedClass(locationData)
      .map {
        PsiLocation.fromPsiElement[PsiClass](project, _)
      }.foreach {
      res.add(_)
    }
    res
  }

  private def createLocationFor(project: Project, psiFile: PsiFile, lineNum: Int,
                                withName: Option[String] = None): Location[_ <: PsiElement] = {
    assert(lineNum > 0)
    val doc: Document = PsiDocumentManager.getInstance(project).getDocument(psiFile)
    if (doc == null) {
      return null
    }
    val lineCount: Int = doc.getLineCount
    var lineStartOffset: Int = 0
    var endOffset: Int = 0
    if (lineNum <= lineCount) {
      lineStartOffset = doc.getLineStartOffset(lineNum - 1)
      endOffset = doc.getLineEndOffset(lineNum - 1)
    }
    else {
      lineStartOffset = 0
      endOffset = doc.getTextLength
    }
    var offset: Int = lineStartOffset
    var elementAtLine: PsiElement = null
    var found = false
    while (offset <= endOffset && !found) {
      elementAtLine = psiFile.findElementAt(offset)
      if (!elementAtLine.isInstanceOf[PsiWhiteSpace]) {
        found = true
      }
      val length: Int = elementAtLine.getTextLength
      offset += (if (length > 1) length - 1 else 1)
    }
    withName match {
      case Some(testName) =>
        PsiLocationWithName(project, if (elementAtLine != null) elementAtLine else psiFile, testName)
      case _ =>
        PsiLocation.fromPsiElement(project, if (elementAtLine != null) elementAtLine else psiFile)
    }
  }
}
