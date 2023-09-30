package js7.base.utils

import scala.annotation.tailrec

object BinarySearch:

  def binarySearch(indexedSeq: IndexedSeq[Int], value: Int): (Int, Boolean) =
    binarySearch(0, indexedSeq.length, indexedSeq(_).compare(value))

  def binarySearch(start: Int, end: Int, compareTo: Int => Int): (Int, Boolean) =
    @tailrec def loop(low: Int, high: Int): (Int, Boolean) =
      if low <= high then
        val middle = (low + high) >>> 1
        val cmp = compareTo(middle)
        if cmp < 0 then
          loop(low + 1, high)
        else if cmp > 0 then
          loop(low, high - 1)
        else
          middle -> true
      else
        low -> false

    loop(start, end - 1)
