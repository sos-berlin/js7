package js7.launcher.utils

import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.configuration.JobLauncherConf.ErrLineLengthMaximum

private[utils] final class LastLineKeeper
{
  private var _lastErrLine = ""
  private var nonEmpty = false
  private var hasLineEnd = false

  def lastErrLine: Option[String] =
    nonEmpty ? _lastErrLine.trim.truncateWithEllipsis(ErrLineLengthMaximum)

  private[utils] def testLastErrLine = _lastErrLine

  def put(chunk: String): Unit =
    if (chunk.nonEmpty) {
      nonEmpty = true
      if (hasLineEnd) _lastErrLine = ""
      val n = chunk.lastIndexOf('\n')
      if (n == -1) {
        hasLineEnd = false
        set(if (hasLineEnd) chunk else _lastErrLine + chunk)
      } else {
        val startOfLine = chunk.substring(n + 1)
        if (!startOfLine.forall(_.isSpaceChar)) {
          set(if (hasLineEnd) _lastErrLine + startOfLine else startOfLine)
        } else {
          val n0 = chunk.lastIndexOf('\n', n - 1)
          if (n0 == -1) {
            set(_lastErrLine + chunk.substring(0, n + 1))
          } else {
            set(chunk.substring(n0 + 1, n + 1))
          }
          hasLineEnd = true
        }
      }
    }

  private def set(string: String): Unit =
    _lastErrLine = string.take(ErrLineLengthMaximum + 1)
}
