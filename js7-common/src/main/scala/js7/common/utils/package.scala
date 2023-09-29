package js7.common

/**
  * @author Joacim Zschimmer
  */
package object utils:
  def untilNoneIterator[A](read: => Option[A]): Iterator[A] =
    UntilNoneIterator(read)
