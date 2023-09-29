package js7.base.utils

import java.util.NoSuchElementException
import js7.base.utils.CloseableIterator.*
import scala.util.control.NonFatal

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
    this match {
      case _: CloseAtEnd[A @unchecked] =>
        this

      case _ =>
        if hasNext then
          new CloseAtEnd[A](this)
        else {
          close()
          this
        }
    }

  def :+[B >: A](b: => B) = wrap(this ++ Iterator(b))

  def +:[B >: A](b: => B) = wrap(CloseableIterator.fromIterator(Iterator(b)) ++ this)

  def ++[B >: A](b: => CloseableIterator[B]): CloseableIterator[B] =
    new Concatenated(this, b)

  override def take(n: Int): CloseableIterator[A] =
    wrap(super.take(n))

  override def takeWhile(p: A => Boolean): CloseableIterator[A] =
    wrap(super.takeWhile(p))

  override def drop(n: Int): CloseableIterator[A] =
    wrap(super.drop(n))

  override def dropWhile(p: A => Boolean): CloseableIterator[A] =
    wrap(super.dropWhile(p))

  override def filter(p: A => Boolean): CloseableIterator[A] =
    wrap(super.filter(p))

  override def map[B](f: A => B): CloseableIterator[B] =
    wrap(super.map(f))

  override def flatMap[B](f: A => IterableOnce[B]): CloseableIterator[B] =
    wrap(super.flatMap(f))

  override def tapEach[U](f: A => U): CloseableIterator[A] =
    wrap(super.tapEach(f))

  override def collect[B](pf: PartialFunction[A, B]): CloseableIterator[B] =
    wrap(super.collect[B](pf))

  /** Side-effect after successful close. */
  final def onClosed(sideEffect: => Unit): CloseableIterator[A] =
    new CloseableIterator[A] {
      def close() = {
        CloseableIterator.this.close()
        sideEffect
      }
      def hasNext = CloseableIterator.this.hasNext
      def next() = CloseableIterator.this.next()
      override def toString = CloseableIterator.this.toString
  }

  private def wrap[B](baseIterator: Iterator[B]): CloseableIterator[B] =
    new CloseableIterator[B] {
      def close() = CloseableIterator.this.close()
      def hasNext = baseIterator.hasNext
      def next() = baseIterator.next()
      override def toString = s"CloseableIterator($baseIterator)"
    }
}

object CloseableIterator {
  val empty = new CloseableIterator[Nothing] {
    def close() = {}
    def hasNext = false
    def next() = throw new NoSuchElementException
    override def toString = "CloseableIterator.empty"
  }

  def apply[A](as: A*) = fromIterator(as.iterator)

  def fromCloseable[C <: AutoCloseable, A](closeable: C)(toIterator: C => Iterator[A]): CloseableIterator[A] =
    try fromIterator(toIterator(closeable)) onClosed { closeable.close() }
    catch { case NonFatal(t) =>  // TODO Use AutoClosable.closeOnError
      try closeable.close()
      catch { case NonFatal(tt) if tt ne t => t.addSuppressed(tt) }
      throw t
    }

  def fromIterator[A](baseIterator: Iterator[A]): CloseableIterator[A] =
    new CloseableIterator[A] {
      def close() = {}
      def hasNext = baseIterator.hasNext
      def next() = baseIterator.next()
      override def toString = s"CloseableIterator($baseIterator)"
    }

  private class CloseAtEnd[A](underlying: CloseableIterator[A]) extends CloseableIterator[A]
  {
    def hasNext = underlying.hasNext || {
      underlying.close()
      false
    }

    def next() =
      try underlying.next()
      catch {
        case t: NoSuchElementException =>
          try underlying.close()
          catch {
            case NonFatal(tt) if tt ne t => t.addSuppressed(tt)
          }
          throw t
      }

    def close() = underlying.close()

    override def toString = s"CloseableIterator.CloseAtEnd($underlying)"
  }

  private class Concatenated[A, B >: A](a: CloseableIterator[A], lazyB: => CloseableIterator[B])
  extends CloseableIterator[B]
  {
    private val b = Lazy(lazyB)
    private val concatenated = (a: Iterator[A]) ++ b.value

    def close() =
      try a.close()
      finally for b <- b do b.close()

    def hasNext = concatenated.hasNext

    def next() = concatenated.next()
  }
}
