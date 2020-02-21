package org.jetbrains.plugins.scala.codeInspection.typeLambdaSimplify

import com.intellij.codeInspection.{LocalQuickFix, ProblemDescriptor, ProblemsHolder}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.typeLambdaSimplify.KindProjectorUseCorrectLambdaKeywordInspection._
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, AbstractInspection, InspectionBundle}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScParameterizedTypeElement, ScSimpleTypeElement}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createTypeElementFromText

/**
 * Author: Svyatoslav Ilinskiy
 * Date: 6/25/15
 */
class KindProjectorUseCorrectLambdaKeywordInspection extends AbstractInspection(inspectionName) {

  override def actionFor(implicit holder: ProblemsHolder, isOnTheFly: Boolean): PartialFunction[PsiElement, Any] = {
    case param: ScParameterizedTypeElement if param.kindProjectorPluginEnabled =>
      val useGreekLambda = ScalaCodeStyleSettings.getInstance(param.getProject).REPLACE_LAMBDA_WITH_GREEK_LETTER
      param.children.foreach {
        case simple: ScSimpleTypeElement =>
          simple.getText match {
            case "Lambda" if useGreekLambda =>
              val changeKeywordFix = new KindProjectorUseCorrectLambdaKeywordQuickFix(simple, "λ")
              holder.registerProblem(simple, InspectionBundle.message("kind.projector.replace.lambda.with.lamda.char"), changeKeywordFix)
              val changeSettingsFix = new ChangeLambdaCodeStyleSetting(!useGreekLambda)
              holder.registerProblem(simple, codeStyleSettingUseWordLambda, changeSettingsFix)
            case "λ" if !useGreekLambda =>
              val changeKeywordFix = new KindProjectorUseCorrectLambdaKeywordQuickFix(simple, "Lambda")
              holder.registerProblem(simple, InspectionBundle.message("kind.projector.replace.lambda.char.with.lambda"), changeKeywordFix)
              val changeSettingsFix = new ChangeLambdaCodeStyleSetting(!useGreekLambda)
              holder.registerProblem(simple, codeStyleSettingUseGreekLambda, changeSettingsFix)
            case _ =>
          }
        case _ =>

      }
  }
}

class KindProjectorUseCorrectLambdaKeywordQuickFix(e: PsiElement, replacement: String) extends AbstractFixOnPsiElement(inspectionName, e) {

  override protected def doApplyFix(elem: PsiElement)
                                   (implicit project: Project): Unit = {
    elem.replace(createTypeElementFromText(replacement))
  }
}

class ChangeLambdaCodeStyleSetting(useGreekLambda: Boolean) extends LocalQuickFix {
  override def getFamilyName: String = getName

  override def getName: String =
    if (useGreekLambda) codeStyleSettingUseGreekLambda
    else codeStyleSettingUseWordLambda

  override def applyFix(project: Project, d: ProblemDescriptor): Unit = {
    ScalaCodeStyleSettings.getInstance(project).REPLACE_LAMBDA_WITH_GREEK_LETTER = useGreekLambda
  }
}

object KindProjectorUseCorrectLambdaKeywordInspection {
  val inspectionName: String = InspectionBundle.message("kind.projector.use.correct.lambda.keyword")
  val inspectionId: String = "KindProjectorUseCorrectLambdaKeyword"
  val codeStyleSettingUseGreekLambda: String = InspectionBundle.message("kind.projector.code.style.setting.use.lamda.char")
  val codeStyleSettingUseWordLambda: String = InspectionBundle.message("kind.projector.code.style.setting.use.lamda.word")
}
