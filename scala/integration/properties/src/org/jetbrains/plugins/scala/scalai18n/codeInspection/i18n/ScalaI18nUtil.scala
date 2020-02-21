package org.jetbrains.plugins.scala
package scalai18n
package codeInspection
package i18n

import java.text.MessageFormat

import com.intellij.codeInsight.AnnotationUtil
import com.intellij.lang.properties.psi.PropertiesFile
import com.intellij.lang.properties.{IProperty, PropertiesImplUtil, PropertiesReferenceManager}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.util.Ref
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi._
import org.jetbrains.annotations.{NotNull, Nullable}
import org.jetbrains.plugins.scala.extensions.{PsiMethodExt, ResolvesTo, _}
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScBindingPattern, ScCaseClause}
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScConstructorInvocation, ScLiteral}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDefinition, ScValueOrVariable}
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * @author Ksenia.Sautina
 * @since 7/17/12
 */

object ScalaI18nUtil {
  def mustBePropertyKey(@NotNull literal: ScLiteral,
                        @Nullable annotationAttributeValues: mutable.HashMap[String, AnyRef] = null): Boolean = {
    mayBePropertyKey(literal) && isPassedToAnnotated(literal, AnnotationUtil.PROPERTY_KEY, annotationAttributeValues)
  }

  private def mayBePropertyKey(literal: ScLiteral): Boolean = literal match {
    case ScStringLiteral(string) => !string.exists {
      case '=' | ':' => true
      case character => Character.isWhitespace(character)
    }
    case _ => false
  }

  @tailrec
  def isPassedToAnnotated(@NotNull element: PsiElement, annFqn: String,
                          @Nullable annotationAttributeValues: mutable.HashMap[String, AnyRef] = null): Boolean = {
    def isAnnotated(member: PsiElement): Boolean = isAnnotatedWith(member, annFqn, annotationAttributeValues)
    element.getParent match {
      case argList: ScArgumentExprList =>
        val idx = argList.exprs.indexOf(element)
        if (idx == -1) return false

        argList.getParent match {
          case ScMethodCall(ResolvesTo(method: PsiMethod), _) =>
            isMethodParameterAnnotatedWith(method, idx, annFqn, annotationAttributeValues)
          case ScConstructorInvocation.reference(ResolvesTo(method: PsiMethod)) =>
            isMethodParameterAnnotatedWith(method, idx, annFqn, annotationAttributeValues)
          case _ =>
            false
        }
      case ScInfixExpr(_, ResolvesTo(method: PsiMethod), `element`) =>
        isMethodParameterAnnotatedWith(method, 0, annFqn, annotationAttributeValues)
      case (tuple: ScTuple) && Parent(ScInfixExpr(_, ResolvesTo(method: PsiMethod), arg)) if tuple == arg =>
        val idx = tuple.exprs.indexOf(element)
        if (idx == -1) return false
        isMethodParameterAnnotatedWith(method, idx, annFqn, annotationAttributeValues)
      case assign: ScAssignment =>
        assign
          .resolveAssignment
          .orElse(assign.leftExpression.asOptionOf[ScReferenceExpression].flatMap(_.bind()))
          .exists {
            case ScalaResolveResult(f: ScFunction, _) =>
              f.parameters.headOption.exists(isAnnotated)
            case ScalaResolveResult(e, _) =>
              isAnnotated(e)
            case _ =>
              false
          }
      case block: ScBlock if block.lastStatement.contains(element) =>
        isPassedToAnnotated(block, annFqn, annotationAttributeValues)
      case ScReturn.of(method) => isAnnotated(method)
      case f: ScFunctionDefinition => isAnnotated(f)
      case v: ScValueOrVariable => isAnnotated(v)
      case parenthesised: ScParenthesisedExpr =>
        isPassedToAnnotated(parenthesised, annFqn, annotationAttributeValues)
      case typed: ScTypedExpression if !typed.isSequenceArg =>
        isPassedToAnnotated(typed, annFqn, annotationAttributeValues)
      case matchCase: ScCaseClause =>
        // in match:      caseClause -> caseClauses -> matchExpr (in some other expression)
        // in call block: caseClause -> caseClauses -> block of expression (in argumentList)
        isPassedToAnnotated(matchCase.getParent.getParent, annFqn, annotationAttributeValues)
      case ifExpr: ScIf =>
        isPassedToAnnotated(ifExpr, annFqn, annotationAttributeValues)
      case _ => false
    }
  }

  def isMethodParameterAnnotatedWith(method: PsiMethod,
                                     idx: Int,
                                     annFqn: String,
                                     @Nullable annotationAttributeValues: mutable.HashMap[String, AnyRef]): Boolean = {
    val params = method.parameters
    def varArgsParam = params.lastOption.filter(_.isVarArgs)
    val param: PsiParameter =
      params
        .lift(idx)
        .orElse(varArgsParam)
        .getOrElse(return false)
    isAnnotatedWith(param, annFqn, annotationAttributeValues)
  }

  def isAnnotatedWith(element: PsiElement,
                      annFqn: String,
                      @Nullable annotationAttributeValues: mutable.HashMap[String, AnyRef] = null): Boolean = {
    import ScalaPsiUtil._
    def isDirectAnnotated(element: PsiElement): Boolean =
      isDirectAnnotatedWith(element, annFqn, annotationAttributeValues)
    def isSuperAnnotatedWith(element: PsiNamedElement): Boolean =
      superValsSignatures(element, withSelfType = true).iterator.map(_.namedElement).exists(isDirectAnnotated)
    element match {
      case param: ScClassParameter =>
        isDirectAnnotated(param) || isSuperAnnotatedWith(param)

      case v: ScValueOrVariable =>
        isDirectAnnotated(v) || v.declaredElements.exists(isSuperAnnotatedWith)

      case bindingPattern: ScBindingPattern =>
        isDirectAnnotated(bindingPattern) || isSuperAnnotatedWith(bindingPattern)

      case function: ScFunction =>
        isDirectAnnotated(function) || function.superSignaturesIncludingSelfType.map(_.namedElement).exists(isDirectAnnotated)

      case method: PsiMethod =>
        isDirectAnnotated(method) || method.findSuperMethods().exists(isDirectAnnotated)

      case param: PsiParameter =>
        isDirectAnnotated(param) || {
          param.getDeclarationScope match {
            case method: PsiMethod =>
              val pIndex = param.index
              method
                .findSuperMethods()
                .iterator
                .flatMap(_.parameters.lift(pIndex))
                .exists(isDirectAnnotated)
            case _ =>
              // support more then methods
              false
          }
        }
      case _ =>
        false
    }
  }

  private def isDirectAnnotatedWith(element: PsiElement,
                                    annFqn: String,
                                    @Nullable annotationAttributeValues: mutable.HashMap[String, AnyRef]): Boolean = {
    val annotation: PsiAnnotation = element match {
      case e: PsiModifierListOwner => AnnotationUtil.findAnnotation(e, annFqn)
      case _ => return false
    }
    if (annotation != null) {
      addToAnnotationAttributeValues(annotation, annotationAttributeValues)
      true
    } else false
  }

  private def addToAnnotationAttributeValues(annotation: PsiAnnotation,
                                             @Nullable annotationAttributeValues: mutable.HashMap[String, AnyRef]): Unit = {
    if (annotationAttributeValues != null) {
      val parameterList: PsiAnnotationParameterList = annotation.getParameterList
      val attributes: Array[PsiNameValuePair] = parameterList.getAttributes
      for (attribute <- attributes) {
        val name: String = attribute.getName
        if (annotationAttributeValues.contains(name)) {
          annotationAttributeValues.put(name, attribute.getValue)
        }
      }
    }
  }

  def isPropertyRef(expression: ScLiteral, key: String, resourceBundleName: String): Boolean = {
    if (resourceBundleName == null) {
      !PropertiesImplUtil.findPropertiesByKey(expression.getProject, key).isEmpty
    }
    else {
      val propertiesFiles = propertiesFilesByBundleName(resourceBundleName, expression)
      var containedInPropertiesFile: Boolean = false
      propertiesFiles.forEach { propertiesFile =>
        containedInPropertiesFile |= propertiesFile.findPropertyByKey(key) != null
      }
      containedInPropertiesFile
    }
  }

  @NotNull def propertiesFilesByBundleName(resourceBundleName: String, context: PsiElement): java.util.List[PropertiesFile] = {
    var containingFile: PsiFile = context.getContainingFile
    val containingFileContext: PsiElement = containingFile.getContext
    if (containingFileContext != null) containingFile = containingFileContext.getContainingFile
    var virtualFile: VirtualFile = containingFile.getVirtualFile
    if (virtualFile == null) {
      virtualFile = containingFile.getOriginalFile.getVirtualFile
    }
    if (virtualFile != null) {
      val project: Project = containingFile.getProject
      val module: Module = ProjectRootManager.getInstance(project).getFileIndex.getModuleForFile(virtualFile)
      if (module != null) {
        val refManager: PropertiesReferenceManager = PropertiesReferenceManager.getInstance(project)
        return refManager.findPropertiesFiles(module, resourceBundleName)
      }
    }
    java.util.Collections.emptyList()
  }

  /**
   * Returns number of different parameters in i18n message. For example, for string
   * <i>Class {0} info: Class {0} extends class {1} and implements interface {2}</i>
   * number of parameters is 3.
   *
   * @param expression i18n literal
   * @return number of parameters
   */
  def getPropertyValueParamsMaxCount(expression: ScLiteral): Int = {
    var maxCount: Int = -1
    for (reference <- expression.getReferences) {
      reference match {
        case polyVarRef: PsiPolyVariantReference =>
          for (result <- polyVarRef.multiResolve(false)) {

            if (result.isValidResult && result.getElement.isInstanceOf[IProperty]) {
              val value: String = result.getElement.asInstanceOf[IProperty].getValue
              var format: MessageFormat = null
              try {
                format = new MessageFormat(value)
                val count: Int = format.getFormatsByArgumentIndex.length
                maxCount = Math.max(maxCount, count)
              }
              catch {
                case _: Exception =>
              }
            }
          }
        case _ =>
      }
    }
    maxCount
  }

  def isValidPropertyReference(@NotNull project: Project, @NotNull expression: ScLiteral, @NotNull key: String, @NotNull outResourceBundle: Ref[String]): Boolean = {
    val annotationAttributeValues = new mutable.HashMap[String, AnyRef]
    annotationAttributeValues.put(AnnotationUtil.PROPERTY_KEY_RESOURCE_BUNDLE_PARAMETER, null)
    if (mustBePropertyKey(expression, annotationAttributeValues)) {
      annotationAttributeValues get AnnotationUtil.PROPERTY_KEY_RESOURCE_BUNDLE_PARAMETER exists {
        case bundleName: PsiElement =>
          val result = JavaPsiFacade.getInstance(bundleName.getProject).getConstantEvaluationHelper.computeConstantExpression(bundleName)
          if (result == null) false else {
            val bundleName = result.toString
            outResourceBundle.set(bundleName)
            isPropertyRef(expression, key, bundleName)
          }
        case _ => false
      }
    } else true
  }
}
