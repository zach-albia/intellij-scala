package org.jetbrains.plugins.scala
package lang
package resolve

import com.intellij.openapi.progress.ProgressManager
import org.jetbrains.plugins.scala.caches.DropOn
import org.jetbrains.plugins.scala.lang.psi.api.base._
import org.jetbrains.plugins.scala.macroAnnotations.CachedWithRecursionGuard

import scala.collection.Set

trait ResolvableStableCodeReferenceElement

object ResolvableStableCodeReferenceElement {

  implicit class Ext(val stableRef: ScStableCodeReferenceElement) extends AnyVal {

    @CachedWithRecursionGuard(stableRef, ScalaResolveResult.EMPTY_ARRAY, DropOn.semanticChange(stableRef))
    def resolveTypesOnly(incomplete: Boolean): Array[ScalaResolveResult] = {
      val importResolverNoMethods = new StableCodeReferenceElementResolver(stableRef, false, false, false) {
        override protected def getKindsFor(ref: ScStableCodeReferenceElement): Set[ResolveTargets.Value] = {
          ref.getKinds(incomplete = false) -- StdKinds.methodRef
        }
      }
      importResolverNoMethods.resolve(stableRef, incomplete)
    }

    @CachedWithRecursionGuard(stableRef, ScalaResolveResult.EMPTY_ARRAY, DropOn.semanticChange(stableRef))
    def resolveMethodsOnly(incomplete: Boolean): Array[ScalaResolveResult] = {
      val importResolverNoTypes = new StableCodeReferenceElementResolver(stableRef, false, false, false) {
        override protected def getKindsFor(ref: ScStableCodeReferenceElement): Set[ResolveTargets.Value] = {
          ref.getKinds(incomplete = false) -- StdKinds.stableClass
        }
      }

      importResolverNoTypes.resolve(stableRef, incomplete)
    }

    @CachedWithRecursionGuard(stableRef, ScalaResolveResult.EMPTY_ARRAY, DropOn.semanticChange(stableRef))
    def resolveNoConstructor: Array[ScalaResolveResult] = {
      ProgressManager.checkCanceled()
      val noConstructorResolver = new StableCodeReferenceElementResolver(stableRef, false, false, true)
      noConstructorResolver.resolve(stableRef, incomplete = false)
    }

    @CachedWithRecursionGuard(stableRef, ScalaResolveResult.EMPTY_ARRAY, DropOn.semanticChange(stableRef))
    def resolveAllConstructors: Array[ScalaResolveResult] = {
      ProgressManager.checkCanceled()
      val resolverAllConstructors = new StableCodeReferenceElementResolver(stableRef, false, true, false)
      resolverAllConstructors.resolve(stableRef, incomplete = false)
    }

    @CachedWithRecursionGuard(stableRef, ScalaResolveResult.EMPTY_ARRAY, DropOn.semanticChange(stableRef))
    def shapeResolve: Array[ScalaResolveResult] = {
      ProgressManager.checkCanceled()
      val shapesResolver = new StableCodeReferenceElementResolver(stableRef, true, false, false)
      shapesResolver.resolve(stableRef, incomplete = false)
    }

    @CachedWithRecursionGuard(stableRef, ScalaResolveResult.EMPTY_ARRAY, DropOn.semanticChange(stableRef))
    def shapeResolveConstr: Array[ScalaResolveResult] = {
      ProgressManager.checkCanceled()
      val shapesResolverAllConstructors = new StableCodeReferenceElementResolver(stableRef, true, true, false)
      shapesResolverAllConstructors.resolve(stableRef, incomplete = false)
    }
  }
}
