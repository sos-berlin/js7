package js7.base.utils

import scala.annotation.tailrec

object BinarySearch:

  def binarySearch[A](indexedSeq: IndexedSeq[A])(value: A)(using ordering: Ordering[A])
  : (Int, Boolean) =
    binarySearchNew(0, indexedSeq.length): i =>
      ordering.compare(value, indexedSeq(i))

  def binarySearch[A, B](indexedSeq: IndexedSeq[A], convert: A => B)(value: B)
    (using ordering: Ordering[B])
  : (Int, Boolean) =
    binarySearchNew(0, indexedSeq.length): i =>
      ordering.compare(value, convert(indexedSeq(i)))

  /** Binary search in an ordered sequence.
   * `compareTo` with an index as argument should return
   * - 0 if the indexed values equals the searched value.
   * - a negative number if the searched value is less than the indexed value.
   * - a positive number if the searched value is greated than the indexed value.
   * argument is an index
   * @param start the first index
   * @param end the index after the last one.
   * @param compareTo argument is an index */
  def binarySearchNew(start: Int, end: Int)(compareTo: Int => Int): (Int, Boolean) =
    @tailrec def loop(low: Int, high: Int): (Int, Boolean) =
      // high is the index *after*, high - low == length
      if low < high then
        val middle = low + ((high - low) >>> 1)
        val cmp = compareTo(middle)
        if cmp < 0 then
          loop(low, middle)
        else if cmp > 0 then
          loop(middle + 1, high)
        else
          middle -> true
      else
        low -> false

    loop(start, end)
