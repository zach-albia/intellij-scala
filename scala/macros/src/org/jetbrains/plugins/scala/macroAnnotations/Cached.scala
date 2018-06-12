package org.jetbrains.plugins.scala.macroAnnotations

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
  * If you annotate a function with @Cached annotation, the compiler will generate code to cache it.
  *
  * If an annotated function has parameters, one field will be generated (a HashMap).
  * If an annotated function has no parameters, two fields will be generated: result and modCount
  *
  * NOTE !IMPORTANT!: function annotated with @Cached must be on top-most level because generated code generates fields
  * right outside the cached function and if this function is inner it won't work.
  *
  * Author: Svyatoslav Ilinskiy
  * Date: 9/18/15.
  */
class Cached(modTracker: Object) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro Cached.cachedImpl
}

object Cached {
  def cachedImpl(c: whitebox.Context)(annottees: c.Tree*): c.Expr[Any] = {
    import CachedMacroUtil._
    import c.universe._
    implicit val x: c.type = c

    def abort(message: String) = c.abort(c.enclosingPosition, message)

    //from annotation parameter
    val modTracker = c.prefix.tree match {
      case q"new Cached(..$params)" if params.length == 1 =>
        params.head.asInstanceOf[c.universe.Tree]
      case _ => abort("Wrong parameters")
    }

    annottees.toList match {
      case DefDef(mods, name, tpParams, paramss, retTp, rhs) :: Nil =>
        if (retTp.isEmpty) {
          abort("You must specify return type")
        }
        //generated names
        val cachedFunName = generateTermName(name.toString + "$original")
        val cacheField = generateTermName(name.toString + "$cache")

        //DefDef parameters
        val flatParams = paramss.flatten
        val paramNames = flatParams.map(_.name)
        val hasParameters: Boolean = flatParams.nonEmpty

        val hasReturnStmts = hasReturnStatements(c)(rhs)

        val cachedFun =
          if (hasReturnStmts) q"def $cachedFunName(): $retTp = $rhs" else EmptyTree

        val computation =
          if (hasReturnStmts) q"$cachedFunName()" else q"$rhs"

        val (field, updatedRhs) = if (hasParameters) {
          val paramTypes = flatParams.map(_.tpt)
          val keyType =  tq"(..$paramTypes)"

          val field =
            q"private val $cacheField: $atomicStampedMapTypeFQN[$keyType, $retTp] = $atomicStampedMapFQN[$keyType, $retTp]"

          def updatedRhs = q"""
             ..$cachedFun

             val currModCount = $modTracker.getModificationCount()

             val key = (..$paramNames)

             $cacheField.getOrClear(currModCount, key) match {
               case Some(v) => v
               case None =>
                 val stackStamp = $recursionManagerFQN.markStack()

                 val computed: $retTp = $computation

                 if (stackStamp.mayCacheNow()) {
                   $cacheField.compareAndPut(currModCount, key, computed)
                   computed
                 }
                 else computed
             }
          """
          (field, updatedRhs)
        } else {
          val field =
            q"private val $cacheField: $atomicStampedRefTypeFQN[$retTp] = $atomicStampedRefFQN[$retTp]"

          val updatedRhs =
            q"""
               ..$cachedFun

               val currModCount = $modTracker.getModificationCount()
               val timestamped = $cacheField.timestamped
               val cachedCount = timestamped.modCount

               if (cachedCount == currModCount) timestamped.data
               else {
                 val stackStamp = $recursionManagerFQN.markStack()

                 val computed: $retTp = $computation

                 if (stackStamp.mayCacheNow()) {
                   $cacheField.compareAndSet(cachedCount, currModCount, computed)
                   computed
                 }
                 else computed
               }
             """
          (field, updatedRhs)
        }

        val updatedDef = DefDef(mods, name, tpParams, paramss, retTp, updatedRhs)
        val res = q"""
          ..$field
          $updatedDef
          """
        c.Expr(res)
      case _ => abort("You can only annotate one function!")
    }
  }
}
