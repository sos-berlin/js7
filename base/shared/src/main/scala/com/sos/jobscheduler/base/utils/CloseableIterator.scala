package com.sos.jobscheduler.base.utils

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
    }
}

object CloseableIterator {
  val empty = new CloseableIterator[Nothing] {
    def close() = {}
    def hasNext = false
    def next() = throw new NoSuchElementException
  }

  def fromIterator[A](iterator: Iterator[A]): CloseableIterator[A] =
    new CloseableIterator[A] {
      def close() = {}
      def hasNext = iterator.hasNext
      def next() = iterator.next()
    }
}
