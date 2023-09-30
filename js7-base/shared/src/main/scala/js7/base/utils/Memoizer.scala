package js7.base.utils

/**
  * Memoizes all computed results - fast, but in case of concurrent calls, results may be computed twice.
  * For strictly one computation, see `Memoizer.strict`.
  *
  * @author Joacim Zschimmer
  */
final class Memoizer[A, B](f: A => B, show: Memoizer.Show[A, B]) extends (A => B):

  private val memory = new ScalaConcurrentHashMap[A, B]

  def apply(a: A): B = memory.getOrElseUpdate(a, f(a))

  def size = memory.size

  def toMap = memory.toMap

  override def toString = show.show(this)

object Memoizer:

  trait Show[A, B]:
    def show(memoizer: Memoizer[A, B]): String
  private def defaultShow[A, B]: Show[A, B] =
    m => s"Memoizer(${m.memory.size }" + " results memoized)"

  /**
    * Memoizes all computed results - fast, but in case of concurrent calls, results may be computed twice.
    * For strictly one computation, see `Memoizer.strict`.
    */
  def nonStrict1[A, B](f: A => B)(implicit show: Show[A, B] = defaultShow[A, B]) =
    new Memoizer[A, B](f, show)

  /**
    * Memoizes all computed results - fast, but in case of concurrent calls, results may be computed twice.
    * For strictly one computation, see `Memoizer.strict`.
    */
  def nonStrict2[A, B, R](f: (A, B) => R)(implicit show: Show[(A, B), R] = defaultShow[(A, B), R]) =
    Function.untupled(new Memoizer[(A, B), R](f.tupled, show))

  /**
    * Memoizes all computed results - fast, but in case of concurrent calls, results may be computed twice.
    * For strictly one computation, see `Memoizer.strict`.
    */
  def nonStrict3[A, B, C, R](f: (A, B, C) => R)(implicit show: Show[(A, B, C), R] = defaultShow[(A, B, C), R]) =
    Function.untupled(new Memoizer[(A, B, C), R](f.tupled, show))

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict1[A, B](f: A => B)(implicit show: Show[A, B] = defaultShow[A, B]): A => B =
    val memoizer = new Memoizer[A, B](f, show)
    (a: A) => memoizer.synchronized { memoizer(a) }

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict2[A, B, R](f: (A, B) => R)(implicit show: Show[(A, B), R] = defaultShow[(A, B), R]) =
    Function.untupled(strict1[(A, B), R](f.tupled))

  /**
    * Memoizes all computed results and computes each result strictly once.
    */
  def strict3[A, B, C, R](f: (A, B, C) => R)(implicit show: Show[(A, B, C), R] = defaultShow[(A, B, C), R]) =
    Function.untupled(strict1[(A, B, C), R](f.tupled))
