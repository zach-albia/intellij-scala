package org.jetbrains.plugins.scala.lang

import org.jetbrains.plugins.scala.lang.psi.types.{
  PresentationTypeUpdaters,
  ScLiteralType,
  ScType,
  TypePresentationContext
}

package object refactoring {
  implicit class ScTypePresentationExt(val tpe: ScType) extends AnyVal {
    private[this] def simplifyForPresentation: ScType =
      tpe.recursiveUpdate(PresentationTypeUpdaters.cleanUp)

    private[this] def renderOrPrintLiteral(render: ScType => String): String = tpe match {
      case lit: ScLiteralType => ScLiteralType.printValue(lit)
      case _                  => render(simplifyForPresentation)
    }

    def codeText(implicit ctx: TypePresentationContext): String = renderOrPrintLiteral(_.presentableText)
    def canonicalCodeText: String                               = renderOrPrintLiteral(_.canonicalText)
  }
}
