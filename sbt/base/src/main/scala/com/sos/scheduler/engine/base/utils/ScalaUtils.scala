package com.sos.scheduler.engine.base.utils

import com.sos.scheduler.engine.base.exceptions.PublicException
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

object ScalaUtils {

  def implicitClass[A : ClassTag]: Class[A] = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]

  /**
   * Returns 'body' as a argumentless function with replaced toString.
   */
  def functionWithToString[R](lazyString: ⇒ String)(body: ⇒ R): () ⇒ R =
    new (() ⇒ R) {
      def apply() = body
      override def toString() = lazyString
    }

  /**
   * Replaces toString of the monadic function.
   */
  def withToString1[A, R](lazyString: ⇒ String)(function: A ⇒ R): A ⇒ R =
    new (A ⇒ R) {
      def apply(a: A) = function(a)
      override def toString() = lazyString
    }

  object implicits {
    import scala.language.implicitConversions

    implicit class ToStringFunction1[A, R](val delegate: A ⇒ R) {
      def withToString(string: String) = new (A ⇒ R) {
        def apply(a: A) = delegate(a)
        override def toString() = string
      }
    }

    implicit def toJavaFunction[A, B](function: A ⇒ B): java.util.function.Function[A, B] =
      new java.util.function.Function[A, B] {
        def apply(a: A) = function(a)
      }
  }

  implicit class RichThrowable[A <: Throwable](val delegate: A) extends AnyVal {
    def rootCause: Throwable = {
      @tailrec def cause(t: Throwable): Throwable =
        t.getCause match {
          case null ⇒ t
          case o if o == t ⇒ t
          case o ⇒ cause(o)
        }
      cause(delegate)
    }

    def toStringWithCauses: String = {
      val strings = mutable.Buffer[String]()
      var t: Throwable = delegate
      while (t != null) {
        strings += t.toSimplifiedString
        t = t.getCause
      }
      strings mkString ", caused by: "
    }

    def toSimplifiedString: String = {
      lazy val msg = delegate.getMessage
      if (msg != null && msg != "" && (RichThrowable.isIgnorableClass(delegate.getClass) || delegate.isInstanceOf[PublicException]))
        msg
      else
        delegate.toString
    }
  }

  private object RichThrowable {
    private val isIgnorableClass = Set[Class[_ <: Throwable]](
      classOf[IllegalArgumentException],
      classOf[RuntimeException])
  }

  def cast[A: ClassTag](o: Any): A = {
    val a = implicitClass[A]
    if (o == null) throw new NullPointerException(s"${a.getName} expected instead of null")
    if (!(a isAssignableFrom o.getClass)) throw new ClassCastException(s"${o.getClass.getName} is not a ${a.getName}: $o")
    o.asInstanceOf[A]
  }

  def someUnless[A](a: A, none: A): Option[A] =
    if (a == none) None else Some(a)

  implicit class RichAny[A](val delegate: A) extends AnyVal {
    def substitute(substitution: (A, A)): A = substitute(substitution._1, substitution._2)

    @inline def substitute(when: A, _then: ⇒ A): A =
      if (delegate == when) _then else delegate
  }

  implicit class SwitchOnAtomicBoolean(val delegate: AtomicBoolean) extends AnyVal {
    def switchOn(body: ⇒ Unit) = if (delegate.compareAndSet(false, true)) body
    def switchOff(body: ⇒ Unit) = if (delegate.compareAndSet(true, false)) body
  }

  implicit class RichPartialFunction[A, B](val delegate: PartialFunction[A, B]) extends AnyVal {
    def getOrElse[BB >: B](key: A, default: ⇒ BB): BB = delegate.applyOrElse(key, (_: A) ⇒ default)
  }

  implicit class RichUnitPartialFunction[A](val delegate: PartialFunction[A, Unit]) extends AnyVal {
    def callIfDefined(a: A): Unit = delegate.getOrElse(a, ())
  }

  implicit class SwitchStatement[A](val delegate: A) extends AnyVal {
    def switch[B](pf: PartialFunction[A, Unit]): Unit = pf.callIfDefined(delegate)
  }
}
