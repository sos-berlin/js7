package js7.base.utils

import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops._
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax._
import monix.reactive.Observable
import scala.collection.mutable

/**
  * Splits a stream of UTF-8-encoded ByteArrays into lines, separated by a separator.
  * The ByteSequences must contain whole lines.
  */
final class ByteArrayToLinesObservable[A]
{
  //private var line = mutable.Buffer[ByteArray]()
  private val lines = mutable.Buffer[ByteArray]()

  def apply(bytes: ByteArray): Observable[ByteArray] = {
    if (!bytes.lastOption.contains('\n'))
      Observable.raiseError(Problem(s"Event streaming chunk does not contain whole lines: ${bytes.utf8String.truncateWithEllipsis(50)}").throwable)
    else {
      lines.clear()
      var p = 0
      val length = bytes.length
      while (p < length) {
        bytes.indexOf('\n'.toByte, p) match {
          //case -1 =>  can not happen
          //  line += bytes drop p
          //  p += length
          case i =>
            val slice = bytes.slice(p, i + 1)  // For p == 0 && i + 1 == bytes.length, bytes should be returned
            lines += slice
            //line += slice
            //lines += line.reduce(_ ++ _)  // line.length should be 1, otherwise this is slow
            //line.clear()
            p = i + 1
        }
      }
      Observable.fromIterable(lines)
    }
  }
}
