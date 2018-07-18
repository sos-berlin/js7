package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.CloseableIterator._
import scala.collection.immutable.Seq
import scala.collection.{GenTraversableOnce, Iterator}

/**
  * @author Joacim Zschimmer
  */
trait CloseableIterator[+A] extends Iterator[A] with AutoCloseable
{
  def strict: Seq[A] =
    try super.toVector
    finally close()

  /** Automatically closes the `CloseableIterator` when `hasNext` becomes false. */
  def closeAtEnd: CloseableIterator[A] =
    new CloseAtEnd[A](this)

  def :+[B >: A](b: ⇒ B) = wrap(this ++ Iterator(b))

  def ++[B >: A](b: ⇒ CloseableIterator[B]): CloseableIterator[B] =
    new Concatenated(this, b)

  override def take(n: Int): CloseableIterator[A] =
    wrap(super.take(n))

  override def takeWhile(p: A ⇒ Boolean): CloseableIterator[A] =
    wrap(super.takeWhile(p))

  override def drop(n: Int): CloseableIterator[A] =
    wrap(super.drop(n))

  override def dropWhile(p: A ⇒ Boolean): CloseableIterator[A] =
    wrap(super.dropWhile(p))

  override def filter(p: A ⇒ Boolean): CloseableIterator[A] =
    wrap(super.filter(p))

  override def map[B](f: A ⇒ B): CloseableIterator[B] =
    wrap(super.map(f))

  override def flatMap[B](f: A ⇒ GenTraversableOnce[B]): CloseableIterator[B] =
    wrap(super.flatMap(f))

  override def collect[B](pf: PartialFunction[A, B]): CloseableIterator[B] =
    wrap(super.collect[B](pf))

  private def wrap[B](iterator: Iterator[B]): CloseableIterator[B] =
    new CloseableIterator[B] {
      def close() = CloseableIterator.this.close()
      def hasNext = iterator.hasNext
      def next() = iterator.next()
      override def toString = s"CloseableIterator($iterator)"
    }
}

object CloseableIterator {
  val empty = new CloseableIterator[Nothing] {
    def close() = {}
    def hasNext = false
    def next() = throw new NoSuchElementException
    override def toString = "CloseableIterator.empty"
  }

  def fromIterator[A](iterator: Iterator[A]): CloseableIterator[A] =
    new CloseableIterator[A] {
      def close() = {}
      def hasNext = iterator.hasNext
      def next() = iterator.next()
      override def toString = s"CloseableIterator($iterator)"
    }

  private class CloseAtEnd[A](underlying: CloseableIterator[A]) extends CloseableIterator[A]
  {
    def hasNext = underlying.hasNext || {
      underlying.close()
      false
    }

    def next() = underlying.next()

    def close() = underlying.close()

    override def toString = s"CloseableIterator.CloseAtEnd($underlying)"
  }

  private class Concatenated[A, B >: A](a: CloseableIterator[A], b: ⇒ CloseableIterator[B])
  extends CloseableIterator[B]
  {
    val concatenated = (a: Iterator[A]) ++ b

    def close() =
      try a.close()
      finally b.close()

    def hasNext = concatenated.hasNext

    def next() = concatenated.next()
  }
}
