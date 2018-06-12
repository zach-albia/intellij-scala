package org.jetbrains.plugins.scala.caches.impl;

import org.jetbrains.plugins.scala.caches.AtomicStampedRef;
import org.jetbrains.plugins.scala.caches.AtomicStampedRef.Timestamped;

import java.util.concurrent.atomic.AtomicLongFieldUpdater;

/**
 * Nikolay.Tropin
 * 12-Jun-18
 */
public class AtomicStampedRefImpl<V> implements AtomicStampedRef<V> {
  private static final AtomicLongFieldUpdater<AtomicStampedRefImpl> stampUpdater =
          AtomicLongFieldUpdater.newUpdater(AtomicStampedRefImpl.class, "stamp");

  private volatile long stamp = -1;

  protected volatile V value = null;

  private static long ACCESSED = Long.MIN_VALUE;

  protected long startAccess() {
    long currentStamp;
    do {
      currentStamp = stamp;
    } while (currentStamp == ACCESSED || !stampUpdater.compareAndSet(this, currentStamp, ACCESSED));

    return currentStamp;
  }

  protected boolean endAccess(long newStamp) {
    return stampUpdater.compareAndSet(this, ACCESSED, newStamp);
  }

  @Override
  public Timestamped<V> timestamped() {
    long currentStamp = startAccess();

    V currentValue = value;

    endAccess(currentStamp);

    return new Timestamped<V>(currentValue, currentStamp);
  }

  @Override
  public void compareAndSet(long expectedStamp, long newStamp, V newValue) {
    long currentStamp = startAccess();

    if (expectedStamp == currentStamp) {
      value = newValue;
    }

    endAccess(newStamp);
  }
}