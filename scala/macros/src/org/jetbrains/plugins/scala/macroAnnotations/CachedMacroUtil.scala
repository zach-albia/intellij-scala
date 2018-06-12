package org.jetbrains.plugins.scala.macroAnnotations

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
  * Author: Svyatoslav Ilinskiy
  * Date: 9/22/15.
  */
object CachedMacroUtil {
  val debug: Boolean = false
  //to analyze caches pass in the following compiler flag: "-Xmacro-settings:analyze-caches"
  val ANALYZE_CACHES: String = "analyze-caches"

  def println(a: Any): Unit = {
    if (debug) {
      Console.println(a)
    }
  }

  def cachesUtilFQN(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    q"_root_.org.jetbrains.plugins.scala.caches.CachesUtil"
  }

  def atomicStampedRefTypeFQN(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    tq"_root_.org.jetbrains.plugins.scala.caches.AtomicStampedRef"
  }

  def atomicStampedRefFQN(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    q"_root_.org.jetbrains.plugins.scala.caches.AtomicStampedRef"
  }

  def atomicStampedMapTypeFQN(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    tq"_root_.org.jetbrains.plugins.scala.caches.AtomicStampedMap"
  }

  def atomicStampedMapFQN(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    q"_root_.org.jetbrains.plugins.scala.caches.AtomicStampedMap"
  }

  def defaultValue(c: whitebox.Context)(tp: c.universe.Tree): c.universe.Tree = {
    import c.universe.Quasiquote
    tp match {
      case tq"Boolean" => q"false"
      case tq"Int" => q"0"
      case tq"Long" => q"0L"
      case _ => q"null"
    }
  }

  def cachedValueTypeFQN(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    tq"_root_.com.intellij.psi.util.CachedValue"
  }

  def cachedValueProviderResultTypeFQN(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    tq"_root_.com.intellij.psi.util.CachedValueProvider.Result"
  }

  def keyTypeFQN(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    tq"_root_.com.intellij.openapi.util.Key"
  }

  def recursionGuardFQN(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    q"org.jetbrains.plugins.scala.caches.RecursionManager.RecursionGuard"
  }

  def recursionManagerFQN(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    q"org.jetbrains.plugins.scala.caches.RecursionManager"
  }

  def psiElementType(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    tq"_root_.com.intellij.psi.PsiElement"
  }

  def thisFunctionFQN(name: String)(implicit c: whitebox.Context): c.universe.Tree = {
    import c.universe.Quasiquote
    q"""getClass.getName ++ "." ++ $name"""
  }

  def generateTermName(name: String = "")(implicit c: whitebox.Context): c.universe.TermName = {
    c.universe.TermName(c.freshName(name))
  }

  def generateTypeName(name: String = "")(implicit c: whitebox.Context): c.universe.TypeName = {
    c.universe.TypeName(c.freshName(name))
  }

  def abort(s: String)(implicit c: whitebox.Context): Nothing = c.abort(c.enclosingPosition, s)

  def box(c: whitebox.Context)(tp: c.universe.Tree): c.universe.Tree = {
    import c.universe.Quasiquote
    tp match {
      case tq"Boolean" => tq"java.lang.Boolean"
      case tq"Int" => tq"java.lang.Integer"
      case tq"Long" => tq"java.lang.Long"
      case _ => tp
    }
  }

  def withUIFreezingGuard(c: whitebox.Context)(tree: c.universe.Tree): c.universe.Tree = {
    import c.universe.Quasiquote
    val fqName = q"_root_.org.jetbrains.plugins.scala.util.UIFreezingGuard"
    q"""
        if ($fqName.isAlreadyGuarded) { $tree }
        else $fqName.withResponsibleUI { $tree }
     """
  }

  def doPreventingRecursion(c: whitebox.Context)(computation: c.universe.Tree,
                                                 guard: c.universe.TermName,
                                                 data: c.universe.TermName,
                                                 resultType: c.universe.Tree): c.universe.Tree = {
    import c.universe.Quasiquote

    val needLocalFunction = hasReturnStatements(c)(computation)
    if (needLocalFunction) {
      abort("Annotated function has explicit return statements, function body can't be inlined")(c)
    }

    q"""if ($guard.checkReentrancy($data)) {}
        else {
          val realKey = $guard.createKey($data)

          val (sizeBefore, sizeAfter) = $guard.beforeComputation(realKey)

          try {
            $computation
          }
          finally {
            $guard.afterComputation(realKey, sizeBefore, sizeAfter)
          }
        }
     """
  }
  def hasReturnStatements(c: whitebox.Context)(tree: c.universe.Tree): Boolean = {
    var result = false
    val traverser = new c.universe.Traverser {
      override def traverse(tree: c.universe.Tree): Unit = tree match {
        case c.universe.Return(_) => result = true
        //skip local functions and classes
        case c.universe.DefDef(_, _, _, _, _, _) =>
        case c.universe.ClassDef(_, _, _, _) =>
        case c.universe.ModuleDef(_, _, _) =>
        case _ => super.traverse(tree)
      }
    }
    traverser.traverse(tree)
    result
  }

  def handleProbablyRecursiveException(c: whitebox.Context)
                                      (elemName: c.universe.TermName,
                                       dataName: c.universe.TermName,
                                       keyName: c.universe.TermName,
                                       calculation: c.universe.Tree): c.universe.Tree = {
    import c.universe.Quasiquote

    q"""
        try {
          $calculation
        }
        catch {
          case exc: org.jetbrains.plugins.scala.caches.CachesUtil.ProbablyRecursionException[_] if exc.key == $keyName =>
            if (exc.elem == $elemName && exc.data == $dataName) {
              try {
                $calculation
              } finally {
                exc.set.foreach(_.setProbablyRecursive(false))
              }
            }
            else {
              val fun = com.intellij.psi.util.PsiTreeUtil.getContextOfType($elemName, true,
                classOf[org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction])
              if (fun == null || fun.isProbablyRecursive) throw exc
              else {
                fun.setProbablyRecursive(true)
                throw exc.copy(set = exc.set + fun)
              }
            }
        }
     """
  }

  def getOrCreateKey(c: whitebox.Context, hasParams: Boolean)
                    (keyId: c.universe.Tree, dataType: c.universe.Tree, resultType: c.universe.Tree)
                    (implicit c1: c.type): c.universe.Tree = {

    import c.universe.Quasiquote

    if (hasParams) q"$cachesUtilFQN.getOrCreateKey[$cachesUtilFQN.CachedMap[$dataType, $resultType]]($keyId)"
    else q"$cachesUtilFQN.getOrCreateKey[$cachesUtilFQN.CachedRef[$resultType]]($keyId)"
  }

  def getOrCreateKeyStamped(c: whitebox.Context, hasParams: Boolean)
                    (keyId: String, dataType: c.universe.Tree, resultType: c.universe.Tree)
                    (implicit c1: c.type): c.universe.Tree = {

    import c.universe.Quasiquote

    if (hasParams) q"$cachesUtilFQN.getOrCreateKey[$atomicStampedMapTypeFQN[$dataType, $resultType]]($keyId)"
    else           q"$cachesUtilFQN.getOrCreateKey[$atomicStampedRefTypeFQN[$resultType]]($keyId)"
  }

  def getFromStampedMapOrCompute(c: whitebox.Context)
                                (cacheHolder: c.universe.Tree,
                                 modTracker: c.universe.Tree,
                                 computation: c.universe.Tree,
                                 returnType: c.universe.Tree,
                                 paramNames: Seq[c.universe.TermName]): c.universe.Tree = {

    import c.universe.Quasiquote
    implicit val ctx: c.type = c

    q"""
        val currModCount = $modTracker.getModificationCount()

        val key = (..$paramNames)

        $cacheHolder.getOrClear(currModCount, key) match {
          case Some(v) => v
          case None =>
            val stackStamp = $recursionManagerFQN.markStack()

            val computed: $returnType = $computation

            if (stackStamp.mayCacheNow()) {
              $cacheHolder.compareAndPut(currModCount, key, computed)
              computed
            }
            else computed
        }"""

  }

  def getFromStampedRefOrCompute(c: whitebox.Context)
                                (cacheHolder: c.universe.Tree,
                                 modTracker: c.universe.Tree,
                                 computation: c.universe.Tree,
                                 returnType: c.universe.Tree): c.universe.Tree = {

    import c.universe.Quasiquote
    implicit val ctx: c.type = c

    q"""
        val currModCount = $modTracker.getModificationCount()
        val timestamped = $cacheHolder.timestamped
        val cachedCount = timestamped.modCount

        if (cachedCount == currModCount) timestamped.data
        else {
          val stackStamp = $recursionManagerFQN.markStack()

          val computed: $returnType = $computation

          if (stackStamp.mayCacheNow()) {
            $cacheHolder.compareAndSet(cachedCount, currModCount, computed)
            computed
          }
          else computed
        }"""

  }

}