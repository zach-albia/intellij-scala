package org.jetbrains.plugins.scala.caches.impl

import org.jetbrains.plugins.scala.caches.AtomicStampedMap

/**
  * Nikolay.Tropin
  * 12-Jun-18
  */
class AtomicStampedMapImpl[K, V] extends AtomicStampedRefImpl[Map[K, V]] with AtomicStampedMap[K, V] {

  override def compareAndPut(expectedStamp: Long, newKey: K, newValue: V): Boolean = {
    val currentStamp = startAccess()

    if (currentStamp == expectedStamp) {
      value = value.updated(newKey, newValue)
    }

    endAccess(currentStamp)
  }

  def getOrClear(expectedStamp: Long, key: K): Option[V] = {
    val currentStamp = startAccess()

    if (currentStamp != expectedStamp) {
      value = Map.empty
    }
    val result = value.get(key)

    endAccess(expectedStamp)

    result
  }

}
