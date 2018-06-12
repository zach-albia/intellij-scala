package org.jetbrains.plugins.scala.macroAnnotations

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
  * This annotation makes the compiler generate code that caches values in the user data.
  *
  * UserDataHolder type should have instance of a `org.jetbrains.plugins.scala.caches.ProjectUserDataHolder` type class
  *
  * Caches are invalidated on change of `dependencyItem`.
  *
  * Author: Svyatoslav Ilinskiy, Nikolay.Tropin
  * Date: 9/25/15.
  */
class CachedInUserData(userDataHolder: Any, dependencyItem: Object) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro CachedInUserData.cachedInsideUserDataImpl
}

object CachedInUserData {
  def cachedInsideUserDataImpl(c: whitebox.Context)(annottees: c.Tree*): c.Expr[Any] = {
    import CachedMacroUtil._
    import c.universe._
    implicit val x: c.type = c
    def parameters: (Tree, Tree) = {
      c.prefix.tree match {
        case q"new CachedInUserData(..$params)" if params.length == 2 =>
          (params(0), params(1))
        case _ => abort("Wrong annotation parameters!")
      }
    }

    //annotation parameters
    val (elem, modTracker) = parameters

    annottees.toList match {
      case DefDef(mods, name, tpParams, paramss, retTp, rhs) :: Nil =>
        if (retTp.isEmpty) {
          abort("You must specify return type")
        }
        //function parameters
        val flatParams = paramss.flatten
        val parameterTypes = flatParams.map(_.tpt)
        val parameterNames: List[c.universe.TermName] = flatParams.map(_.name)
        val hasParams = flatParams.nonEmpty

        //generated types
        val dataType = if (hasParams) tq"(..$parameterTypes)" else tq"Unit"
        val resultType = box(c)(retTp)

        //generated names
        val keyId = c.freshName(name.toString + "cacheKey")
        val cachedFunName = generateTermName(name.toString + "$original")

        val key = getOrCreateKeyStamped(c, hasParams)(keyId, dataType, resultType)
        val getOrCreateCachedHolder =
          if (hasParams)
            q"$cachesUtilFQN.getOrCreateStampedMap[$dataType, $resultType]($elem, $key)"
          else
            q"$cachesUtilFQN.getOrCreateStampedRef[$resultType]($elem, $key)"

        val hasReturnStmts = hasReturnStatements(c)(rhs)
        val withUIGuard = withUIFreezingGuard(c)(rhs)

        val cachedFun =
          if (hasReturnStmts) q"def $cachedFunName(): $retTp = $withUIGuard" else EmptyTree

        val computation =
          if (hasReturnStmts) q"$cachedFunName()" else q"$withUIGuard"

        val getFromCacheOrCompute =
          if (hasParams)
            getFromStampedMapOrCompute(c)(getOrCreateCachedHolder, modTracker, computation, resultType, parameterNames)
          else
            getFromStampedRefOrCompute(c)(getOrCreateCachedHolder, modTracker, computation, resultType)


        val updatedRhs = q"""
          ..$cachedFun

          $getFromCacheOrCompute
          """
        val updatedDef = DefDef(mods, name, tpParams, paramss, retTp, updatedRhs)

        c.Expr(q"..$updatedDef")
      case _ => abort("You can only annotate one function!")
    }
  }
}
