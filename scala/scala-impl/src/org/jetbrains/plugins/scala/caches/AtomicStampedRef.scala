package org.jetbrains.plugins.scala.caches

import org.jetbrains.plugins.scala.caches.CachesUtil.Timestamped
import org.jetbrains.plugins.scala.caches.impl.AtomicStampedRefImpl

/**
  * Nikolay.Tropin
  * 12-Jun-18
  */
trait AtomicStampedRef[V] {
  def snapshot: Timestamped[V]

  def setIfExpectedStamp(expectedStamp: Long, newStamp: Long, newValue: V): Unit

    def doIfExpectedStamp(expected: Long, newStamp: Long)(action: => Unit): Unit
}

object AtomicStampedRef {
  def apply[V](): AtomicStampedRef[V] = new AtomicStampedRefImpl[V]()

  def apply[V](initial: V): AtomicStampedRef[V] = new AtomicStampedRefImpl[V](initial)
}
