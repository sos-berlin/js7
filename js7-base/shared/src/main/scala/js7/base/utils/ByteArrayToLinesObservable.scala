package js7.base.utils

import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops._
import monix.reactive.Observable
import scala.collection.mutable

/**
  * Splits a stream of UTF-8-encoded ByteArrays into lines, separated by a separator.
  * The ByteSequences must contain whole lines.
  */
final class ByteArrayToLinesObservable
extends (ByteArray => Observable[ByteArray])
{
  private val lines = mutable.Buffer.empty[ByteArray]
  private lazy val startedLine = mutable.ArrayBuffer.empty[ByteArray]

  def apply(bytes: ByteArray): Observable[ByteArray] =
    if (bytes.isEmpty)
      Observable.empty
    else {
      var p = 0
      val length = bytes.length
      while (p < length) {
        bytes.indexOf('\n'.toByte, p) match {
          case -1 =>
            startedLine += bytes.slice(p, length)
            p = length

          case i =>
            val slice = bytes.slice(p, i + 1)  /*For p == 0 && i + 1 == bytes.length, no copy is needed*/
            lines += ByteArray.combineAll(startedLine) ++ slice
            startedLine.clear()
            p = i + 1
        }
      }
      val result = Observable.fromIteratorUnsafe(lines.toArray.iterator)
      lines.clear()
      result
    }
}
