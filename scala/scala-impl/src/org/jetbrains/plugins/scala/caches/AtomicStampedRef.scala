package org.jetbrains.plugins.scala.caches

import org.jetbrains.plugins.scala.caches.AtomicStampedRef.Timestamped
import org.jetbrains.plugins.scala.caches.impl.AtomicStampedRefImpl

/**
  * Nikolay.Tropin
  * 12-Jun-18
  */
trait AtomicStampedRef[V] {
  def timestamped: Timestamped[V]

  def compareAndSet(expectedStamp: Long, newStamp: Long, newValue: V): Unit
}

object AtomicStampedRef {

  def apply[V]: AtomicStampedRef[V] = new AtomicStampedRefImpl[V]()

  //Tuple2 class doesn't have half-specialized variants, so (T, Long) almost always have boxed long inside
  case class Timestamped[@specialized(Boolean, Int, AnyRef) T](data: T, modCount: Long)
}
