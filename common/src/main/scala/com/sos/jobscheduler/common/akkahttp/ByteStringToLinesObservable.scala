package com.sos.jobscheduler.common.akkahttp

import akka.util.ByteString
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.Strings.RichString
import java.nio.charset.StandardCharsets.UTF_8
import monix.reactive.Observable
import scala.collection.mutable.ArrayBuffer

/**
  * Splits a stream of UTF-8-encoded ByteStrings into lines, separated by a separator.
  * The ByteStrings must contain whole lines.
  */
final class ByteStringToLinesObservable
extends (ByteString => Observable[String])
{
  private val line = new StringBuilder
  private val lines = new ArrayBuffer[String]

  def apply(byteString: ByteString) =
    if (!byteString.lastOption.contains('\n'))
      Observable.raiseError(Problem(s"Event streaming chunk does not contain whole lines: ${byteString.utf8String.truncateWithEllipsis(50)}").throwable)
    else {
      lines.clear()
      val string = byteString.decodeString(UTF_8)
      var p = 0
      while (p < string.length) {
        string.indexOf('\n', p) match {
          case -1 =>
            line ++= string.substring(p)
            p += string.length
          case i =>
            line ++= string.substring(p, i + 1)  // When separator is included (i+1), substring can return the whole string
            lines += line.toString
            line.clear()
            p = i + 1
        }
      }
      Observable.fromIterable(lines)
    }
}
