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
        val cachedFunName      = generateTermName(name.toString + "$cachedFun")
        val mapAndCounterRef   = generateTermName(name.toString + "$mapAndCounter")
        val timestampedDataRef = generateTermName(name.toString + "$valueAndCounter")

        //DefDef parameters
        val flatParams = paramss.flatten
        val paramNames = flatParams.map(_.name)
        val hasParameters: Boolean = flatParams.nonEmpty

        val hasReturnStmts = hasReturnStatements(c)(rhs)

        val cachedFun =
          if (hasReturnStmts) q"def $cachedFunName(): $retTp = $rhs" else EmptyTree

        val computation =
          if (hasReturnStmts) q"$cachedFunName()" else q"$rhs"

        val (fields, updatedRhs) = if (hasParameters) {
          //wrap type of value in Some to avoid unboxing in putIfAbsent for primitive types
          val mapType = tq"$concurrentMapTypeFqn[(..${flatParams.map(_.tpt)}), _root_.scala.Some[$retTp]]"

          def createNewMap = q"_root_.com.intellij.util.containers.ContainerUtil.newConcurrentMap()"

          val fields = q"""
              new _root_.scala.volatile()
              private val $mapAndCounterRef: $atomicReferenceTypeFQN[$timestampedTypeFQN[$mapType]] =
                new $atomicReferenceTypeFQN($timestampedFQN(null, -1L))
            """

          def updatedRhs = q"""
             ..$cachedFun

             val currModCount = $modTracker.getModificationCount()

             val map = {
               val timestampedMap = $mapAndCounterRef.get
               if (timestampedMap.modCount < currModCount) {
                 $mapAndCounterRef.compareAndSet(timestampedMap, $timestampedFQN($createNewMap, currModCount))
               }
               $mapAndCounterRef.get.data
             }

             val key = (..$paramNames)

             map.get(key) match {
               case Some(v) => v
               case null =>
                 val stackStamp = $recursionManagerFQN.markStack()

                 //null values are not allowed in ConcurrentHashMap, but we want to cache nullable functions
                 val computed: Some[$retTp] = _root_.scala.Some($computation)

                 if (stackStamp.mayCacheNow()) {
                   val race = map.putIfAbsent(key, computed)
                   if (race != null) race.get
                   else computed.get
                 }
                 else computed.get
             }
          """
          (fields, updatedRhs)
        } else {
          val fields = q"""
              new _root_.scala.volatile()
              private val $timestampedDataRef: $atomicReferenceTypeFQN[$timestampedTypeFQN[$retTp]] =
                new $atomicReferenceTypeFQN($timestampedFQN(${defaultValue(c)(retTp)}, -1L))
            """

          val updatedRhs =
            q"""
               ..$cachedFun

               val currModCount = $modTracker.getModificationCount()

               val timestamped = $timestampedDataRef.get
               if (timestamped.modCount == currModCount) timestamped.data
               else {
                 val stackStamp = $recursionManagerFQN.markStack()

                 val computed: $retTp = $computation

                 if (stackStamp.mayCacheNow()) {
                   $timestampedDataRef.compareAndSet(timestamped, $timestampedFQN(computed, currModCount))
                   $timestampedDataRef.get.data
                 }
                 else computed
               }
             """
          (fields, updatedRhs)
        }

        val updatedDef = DefDef(mods, name, tpParams, paramss, retTp, updatedRhs)
        val res = q"""
          ..$fields
          $updatedDef
          """
        c.Expr(res)
      case _ => abort("You can only annotate one function!")
    }
  }
}
