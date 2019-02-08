package org.jetbrains.plugins.scala.caches.impl;

import org.jetbrains.plugins.scala.caches.AtomicStampedRef;
import org.jetbrains.plugins.scala.caches.CachesUtil.Timestamped;
import scala.Function0;
import scala.runtime.BoxedUnit;

import java.util.concurrent.atomic.AtomicLongFieldUpdater;

/**
 * Nikolay.Tropin
 * 12-Jun-18
 */
public class AtomicStampedRefImpl<V> implements AtomicStampedRef<V> {
  private static final AtomicLongFieldUpdater<AtomicStampedRefImpl> stampUpdater =
          AtomicLongFieldUpdater.newUpdater(AtomicStampedRefImpl.class, "stamp");

  public AtomicStampedRefImpl() {
  }

  public AtomicStampedRefImpl(V value) {
    this.value = value;
  }

  private volatile long stamp = -1;

  protected volatile V value = null;

  private static long ACCESSED = Long.MIN_VALUE;

  private long startAccess() {
    long currentStamp;
    do {
      currentStamp = stamp;
    } while (currentStamp == ACCESSED || !stampUpdater.compareAndSet(this, currentStamp, ACCESSED));

    return currentStamp;
  }

  private boolean endAccess(long newStamp) {
    return stampUpdater.compareAndSet(this, ACCESSED, newStamp);
  }

  @Override
  public Timestamped<V> snapshot() {
    long currentStamp = startAccess();

    V currentValue = value;

    endAccess(currentStamp);

    return new Timestamped<V>(currentValue, currentStamp);
  }

  @Override
  public void setIfExpectedStamp(long expectedStamp, long newStamp, V newValue) {
    long currentStamp = startAccess();

    if (expectedStamp == currentStamp) {
      value = newValue;
    }

    endAccess(newStamp);
  }

  @Override
  public void doIfExpectedStamp(long expectedStamp, long newStamp, Function0<BoxedUnit> action) {
    long currentStamp = startAccess();

    if (expectedStamp == currentStamp) {
      action.apply();
    }

    endAccess(newStamp);
  }
}