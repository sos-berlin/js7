package js7.base.log.reader

import java.time.Instant
import js7.base.log.LogLevel
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeExtensions.toEpochNano
import scala.util.control.NonFatal

/** Byte position in a log file.
  *
  * @param logLevel Info or Debug
  * @param fileInstant Instant of the file
  * @param position Position in the file
  */
final case class LogLineKey private[log](logLevel: LogLevel, fileInstant: Instant, position: Long):

  override def toString = asString

  def asString: String =
    s"$logLevel/${fileInstant.toEpochNano.toDecimalString}/$position"


object LogLineKey:
  private val KeyRegex = """([A-Za-z]+)/(\d+(?:[.]\d+)?)/(\d+)""".r

  def parse(string: String): Checked[LogLineKey] =
    try
      string match
        case KeyRegex(logLevel: String, instant: String, position: String) =>
          Right:
            LogLineKey(
              LogLevel(logLevel),
              Instant.ofEpochSecond((BigDecimal(instant) / 1_000_000_000).toLong),
              position.toLong)
        case _ => throw new RuntimeException
    catch
      case NonFatal(_) => Left(Problem(s"Invalid LogLineKey: $string"))
