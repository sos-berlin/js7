package com.sos.scheduler.engine.common.scalautil

/**
  * Memoizes all computed results - fast, but in case of concurrent calls, results may be computed twice.
  * For strictly one computation, see `ConcurrentMemoizer.strict`.
  *
  * @author Joacim Zschimmer
  */
final class ConcurrentMemoizer[A, B](f: A ⇒ B) extends (A ⇒ B) {

  private val memory = new ScalaConcurrentHashMap[A, B]

  def apply(a: A): B = memory.getOrElseUpdate(a, f(a))

  override def toString = s"ConcurrentMemoizer($f, ${memory.size} results memoized)"
}

object ConcurrentMemoizer {
  def apply[A, B](f: A ⇒ B) = new ConcurrentMemoizer[A, B](f)

  def apply[A, B, R](f: (A, B) ⇒ R) = Function.untupled(new ConcurrentMemoizer[(A, B), R](f.tupled))

  def apply[A, B, C, R](f: (A, B, C) ⇒ R) = Function.untupled(new ConcurrentMemoizer[(A, B, C), R](f.tupled))

  def apply[A, B, C, D, R](f: (A, B, C, D) ⇒ R) = Function.untupled(new ConcurrentMemoizer[(A, B, C, D), R](f.tupled))

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict[A, B](f: A ⇒ B): A ⇒ B = {
    val memoizer = new ConcurrentMemoizer[A, B](f)
    (a: A) ⇒ memoizer.synchronized { memoizer(a) }
  }

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict[A, B, R](f: (A, B) ⇒ R) = Function.untupled(strict[(A, B), R](f.tupled))

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict[A, B, C, R](f: (A, B, C) ⇒ R) = Function.untupled(strict[(A, B, C), R](f.tupled))

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict[A, B, C, D, R](f: (A, B, C, D) ⇒ R) = Function.untupled(strict[(A, B, C, D), R](f.tupled))
}
