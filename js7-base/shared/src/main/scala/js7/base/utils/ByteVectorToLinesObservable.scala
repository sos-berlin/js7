package js7.base.utils

import js7.base.problem.Problem
import js7.base.utils.ScodecUtils._
import js7.base.utils.ScalaUtils.syntax._
import monix.reactive.Observable
import scala.collection.mutable.ArrayBuffer
import scodec.bits.ByteVector

/**
  * Splits a stream of UTF-8-encoded ByteVectors into lines, separated by a separator.
  * The ByteVectors must contain whole lines.
  */
final class ByteVectorToLinesObservable
extends (ByteVector => Observable[ByteVector])
{
  private var line = ByteVector.empty
  private val lines = new ArrayBuffer[ByteVector]

  def apply(byteVector: ByteVector): Observable[ByteVector] =
    if (!byteVector.lastOption.contains('\n'))
      Observable.raiseError(Problem(s"Event streaming chunk does not contain whole lines: ${byteVector.utf8String.truncateWithEllipsis(50)}").throwable)
    else {
      lines.clear()
      var p = 0L
      val length = byteVector.length
      while (p < length) {
        byteVector.indexOf('\n'.toByte, p) match {
          case -1 =>
            line ++= byteVector drop p
            p += length
          case i =>
            line ++= byteVector.slice(p, i + 1)  // When separator is included (i+1), substring can return the whole string
            lines += line.unbuffer
            line = ByteVector.empty
            p = i + 1
        }
      }
      Observable.fromIterable(lines)
    }
}
