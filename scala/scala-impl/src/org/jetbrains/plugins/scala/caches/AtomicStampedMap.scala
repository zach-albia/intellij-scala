package org.jetbrains.plugins.scala.caches

import org.jetbrains.plugins.scala.caches.impl.AtomicStampedMapImpl

/**
  * Nikolay.Tropin
  * 12-Jun-18
  */
trait AtomicStampedMap[K, V] extends AtomicStampedRef[Map[K, V]] {
  def getOrClear(stamp: Long, key: K): Option[V]

  def compareAndPut(expectedStamp: Long, newKey: K, newValue: V): Boolean
}

object AtomicStampedMap {
  def apply[K, V]: AtomicStampedMap[K, V] = new AtomicStampedMapImpl[K, V]()
}