package js7.base.log.reader

import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import js7.base.log.LogLevel
import js7.base.problem.{Checked, Problem}
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.utils.JavaVectors.vectorIndexOf
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

  def parse(byteSeq: fs2.Chunk[Byte]): Checked[LogLineKey] =
    parse(byteSeq.toArray)

  def parse(string: String): Checked[LogLineKey] =
    parse(string.getBytes(UTF_8))

  def parse(array: Array[Byte]): Checked[LogLineKey] =
    try
      val end = array.length
      var i = array.vectorIndexOf('/', 0, end)
      val logLevel = LogLevel(new String(array, 0, i, UTF_8))
      i += 1
      var j = array.vectorIndexOf('/', i, end)
      val instant = new String(array, i, j - i, UTF_8)
      j += 1
      val position = new String(array, j, end - j).toLong
      Right:
        LogLineKey(
          logLevel,
          EpochNano.fromDecimalString(instant).toInstant,
          position)
    catch
      case NonFatal(_) => Left(Problem(s"Invalid LogLineKey: ${new String(array, UTF_8)}"))
