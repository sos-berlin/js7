package com.sos.scheduler.engine.common.scalautil

import scala.annotation.tailrec
import scala.reflect.ClassTag

object ScalaUtils {

  def implicitClass[A : ClassTag]: Class[A] = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]

  def withToString[R](lazyString: ⇒ String)(body: ⇒ R): () ⇒ R =
    new (() ⇒ R) {
      def apply() = body
      override def toString() = lazyString
    }

  object implicits {
    implicit class ToStringFunction1[A, R](val delegate: A ⇒ R) {
      def withToString(string: String) = new (A ⇒ R) {
        def apply(a: A) = delegate(a)
        override def toString() = string
      }
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
  }

  def cast[A : ClassTag](o: Any): A = {
    val a = implicitClass[A]
    if (o == null) throw new NullPointerException(s"${a.getName} expected instead of null")
    if (!(a isAssignableFrom o.getClass)) throw new ClassCastException(s"'$o': ${o.getClass.getName} is not a ${a.getName}")
    o.asInstanceOf[A]
  }

  def someUnless[A](a: A, none: A): Option[A] =
    if (a == none) None else Some(a)

  implicit class RichAny[A](val delegate: A) extends AnyVal {
    def substitute(substitution: (A, A)): A = substitute(substitution._1, substitution._2)

    @inline def substitute(when: A, _then: ⇒ A): A =
      if (delegate == when) _then else delegate
  }
}
