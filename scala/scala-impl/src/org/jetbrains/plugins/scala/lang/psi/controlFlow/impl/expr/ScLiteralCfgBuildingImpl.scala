package org.jetbrains.plugins.scala.lang.psi.controlFlow.impl.expr

import org.jetbrains.plugins.scala.lang.psi.api.base._
import org.jetbrains.plugins.scala.lang.psi.controlFlow.CfgBuilder
import org.jetbrains.plugins.scala.lang.psi.controlFlow.cfg.{ExprResult, ResultRequirement}

trait ScLiteralCfgBuildingImpl { this: ScLiteral =>

  protected override def buildActualExpressionControlFlow(rreq: ResultRequirement)
                                                         (implicit builder: CfgBuilder): ExprResult = {
    import java.{lang => jl}



    // TODO: improve this! something exhaustive would be nice
    //       also handle floats/doubles
    val lit = this.getValue match {
      case null              => builder.`null`
      case value: jl.Boolean => builder.boolean(value)
      case value: jl.Number  => builder.int(value.intValue())
      case value: String     => builder.string(value)
      case _                 => builder.any
    }

    rreq.satisfy(lit, noop = true)
  }
}
