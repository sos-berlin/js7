package com.sos.scheduler.engine.common.scalautil

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.reflect.ClassTag

object ScalaUtils {
  // Warum ist das nicht in Scala enthalten?
  def implicitClass[A : ClassTag]: Class[A] =
    implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]

  implicit class RichTraversableOnce[A](val delegate: TraversableOnce[A]) extends AnyVal {
    def toImmutableSeq: immutable.Seq[A] =
      delegate match {
        case o: immutable.Seq[A] ⇒ o
        case _ ⇒ Vector() ++ delegate
      }

    def countEquals: Map[A, Int] =
      delegate.toTraversable groupBy identity map { case (k, v) ⇒ k -> v.size }

    def toKeyedMap[K](toKey: A ⇒ K): Map[K, A] =
      (delegate map { o ⇒ toKey(o) -> o }).toMap
  }

  implicit class RichArray[A](val delegate: Array[A]) extends AnyVal {
    def toImmutableSeq: immutable.Seq[A] =
      Vector() ++ delegate
  }

  implicit class RichJavaIterable[A](val delegate: java.lang.Iterable[A]) extends AnyVal {
    def toImmutableSeq: immutable.Seq[A] =
      Vector() ++ delegate
  }

  implicit class RichJavaIterator[A](val delegate: java.util.Iterator[A]) extends AnyVal {
    def toImmutableSeq: immutable.Seq[A] =
      Vector() ++ delegate
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
    if (!(a isAssignableFrom o.getClass)) throw new ClassCastException(s"${a.getName} expected instead of ${o.getClass}")
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
