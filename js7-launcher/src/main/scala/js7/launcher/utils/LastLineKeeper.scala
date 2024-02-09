package js7.launcher.utils

import cats.effect.IO
import fs2.Pipe
import js7.base.utils.ScalaUtils.syntax.*

private[launcher] final class LastLineKeeper(lengthLengthMax: Int)
extends Pipe[IO, String, String]:

  private var _lastLine = ""
  private var nonEmpty = false
  private var hasLineEnd = false

  def lastLine: Option[String] =
    nonEmpty ? _lastLine.trim.truncateWithEllipsis(lengthLengthMax)

  private[utils] def testLastErrLine =
    _lastLine

  def apply(stream: fs2.Stream[IO, String]): fs2.Stream[IO, String] =
    stream.map: chunk =>
      put(chunk)
      chunk

  private[utils] def put(chunk: String): Unit =
    if chunk.nonEmpty then
      nonEmpty = true
      if hasLineEnd then _lastLine = ""
      val n = chunk.lastIndexOf('\n')
      if n == -1 then
        hasLineEnd = false
        set(if hasLineEnd then chunk else _lastLine + chunk)
      else
        val startOfLine = chunk.substring(n + 1)
        if !startOfLine.forall(_.isSpaceChar) then
          set(if hasLineEnd then _lastLine + startOfLine else startOfLine)
        else
          val n0 = chunk.lastIndexOf('\n', n - 1)
          if n0 == -1 then
            set(_lastLine + chunk.substring(0, n + 1))
          else
            set(chunk.substring(n0 + 1, n + 1))
          hasLineEnd = true

  private def set(string: String): Unit =
    _lastLine = string.take(lengthLengthMax + 1)
