package js7.base.utils

import cats.syntax.monoid.*
import js7.base.data.ByteSequence.ops.*
import js7.base.data.{ByteArray, ByteSequence}
import monix.reactive.Observable
import scala.collection.mutable

/**
  * Splits a stream of UTF-8-encoded ByteSeqs into lines, separated by LF.
  */
final class ByteSequenceToLinesObservable[ByteSeq](
  implicit ByteSeq: ByteSequence[ByteSeq])
extends (ByteSeq => Observable[ByteArray])
{
  private val lines = mutable.Buffer.empty[ByteArray]
  private lazy val startedLine = mutable.ArrayBuffer.empty[ByteSeq]

  def apply(byteSeq: ByteSeq): Observable[ByteArray] =
    if (byteSeq.isEmpty)
      Observable.empty
    else {
      var p = 0
      val length = byteSeq.length

      while (p < length) {
        byteSeq.indexOf('\n'.toByte, p) match {
          case -1 =>
            startedLine += byteSeq.slice(p, length)
            p = length

          case i =>
            val slice = byteSeq.slice(p, i + 1)  /*For p == 0 && i + 1 == bytes.length, no copy is needed*/
            startedLine += slice
            lines += ByteArray.combineByteSequences(startedLine)
            startedLine.clear()
            p = i + 1
        }
      }

      val result =
        if (lines.lengthIs == 1)
          Observable.pure(lines.head)
        else
          Observable.fromIterable(lines.toVector)
      lines.clear()
      result
  }
}
