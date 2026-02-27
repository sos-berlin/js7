package js7.base.log

import java.time.Instant
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeExtensions.toEpochNano
import scala.util.control.NonFatal

final case class LogLineKey private[log](instant: Instant, position: Long):
  override def toString = s"${instant.toEpochNano.toDecimalString}/$position"

object LogLineKey:
  def parse(string: String): Checked[LogLineKey] =
    try
      val parts = string.split('/')
      if parts.length != 2 then throw new Exception
      val instant = Instant.ofEpochSecond(parts(0).toLong)
      val position = parts(1).toLong
      Right(LogLineKey(instant, position))
    catch case NonFatal(_) => Left(Problem(s"Invalid LogLineKey: $string"))
