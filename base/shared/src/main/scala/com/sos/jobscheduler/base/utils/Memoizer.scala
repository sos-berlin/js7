package com.sos.jobscheduler.base.utils

/**
  * Memoizes all computed results - fast, but in case of concurrent calls, results may be computed twice.
  * For strictly one computation, see `Memoizer.strict`.
  *
  * @author Joacim Zschimmer
  */
final class Memoizer[A, B](f: A => B) extends (A => B) {

  private val memory = new ScalaConcurrentHashMap[A, B]

  def apply(a: A): B = memory.getOrElseUpdate(a, f(a))

  override def toString = s"Memoizer($f, ${memory.size} results memoized)"
}

object Memoizer {
  /**
    * Memoizes all computed results - fast, but in case of concurrent calls, results may be computed twice.
    * For strictly one computation, see `Memoizer.strict`.
    */
  def nonStrict[A, B](f: A => B) = new Memoizer[A, B](f)

  /**
    * Memoizes all computed results - fast, but in case of concurrent calls, results may be computed twice.
    * For strictly one computation, see `Memoizer.strict`.
    */
  def nonStrict[A, B, R](f: (A, B) => R) = Function.untupled(new Memoizer[(A, B), R](f.tupled))

  /**
    * Memoizes all computed results - fast, but in case of concurrent calls, results may be computed twice.
    * For strictly one computation, see `Memoizer.strict`.
    */
  def nonStrict[A, B, C, R](f: (A, B, C) => R) = Function.untupled(new Memoizer[(A, B, C), R](f.tupled))

  /**
    * Memoizes all computed results - fast, but in case of concurrent calls, results may be computed twice.
    * For strictly one computation, see `Memoizer.strict`.
    */
  def nonStrict[A, B, C, D, R](f: (A, B, C, D) => R) = Function.untupled(new Memoizer[(A, B, C, D), R](f.tupled))

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict[A, B](f: A => B): A => B = {
    val memoizer = new Memoizer[A, B](f)
    (a: A) => memoizer.synchronized { memoizer(a) }
  }

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict[A, B, R](f: (A, B) => R) = Function.untupled(strict[(A, B), R](f.tupled))

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict[A, B, C, R](f: (A, B, C) => R) = Function.untupled(strict[(A, B, C), R](f.tupled))

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict[A, B, C, D, R](f: (A, B, C, D) => R) = Function.untupled(strict[(A, B, C, D), R](f.tupled))
}
