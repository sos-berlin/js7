package js7.base.time

import cats.Show
import io.circe.{Decoder, Encoder, Json}
import js7.base.problem.Checked
import js7.base.time.Timestamp._
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait Timestamp extends Ordered[Timestamp] {

  def companion: Companion

  def toEpochMilli: Long

  def toEpochSecond = toEpochMilli / 1000

  /** Returns an ISO-8601 string with milliseconds.
    * For example "2017-12-04T11:22:33.456Z".
    */
  def toIsoString: String

  def format(format: String, maybeTimezone: Option[String] = None): Checked[String]

  //Problem with sbt: def toIsoStringBuilder: StringBuilder

  def compare(o: Timestamp) = toEpochMilli compare o.toEpochMilli

  def +(o: FiniteDuration): Timestamp =
    copy(epochMilli = toEpochMilli + o.toMillis)

  def -(o: FiniteDuration): Timestamp =
    copy(epochMilli = toEpochMilli - o.toMillis)

  def -(o: Timestamp): FiniteDuration =
    new FiniteDuration(toEpochMilli - o.toEpochMilli, MILLISECONDS)

  final def min(o: Timestamp) = if (this < o) this else o

  final def max(o: Timestamp) = if (this > o) this else o

  final def roundToNextSecond: Timestamp = copy((toEpochMilli + 999)/ 1000 * 1000)

  final def secondsOnly: Timestamp = copy(toEpochMilli / 1000 * 1000)

  def roundTo(duration: FiniteDuration): Timestamp = this + duration / 2 roundDownTo duration

  def roundDownTo(duration: FiniteDuration): Timestamp = {
    val durationMillis = duration.toMillis
    companion.ofEpochMilli(toEpochMilli / durationMillis * durationMillis)
  }

  def copy(epochMilli: Long): Timestamp

  def show = toString

  def pretty = toString.replace('T', ' ')

  override def toString = toIsoString
}

object Timestamp
{
  val implementation: Companion = SystemTimestamp
  final val Epoch = ofEpochMilli(0)

  def ofEpochSecond(second: Long): Timestamp =
    ofEpochMilli(second * 1000)

  def ofEpochMilli(milli: Long): Timestamp =
    implementation.ofEpochMilli(milli)

  def apply(dateTime: String): Timestamp =
    implementation.parse(dateTime)

  def parse(dateTime: String): Timestamp =
    implementation.parse(dateTime)

  def ofDeadline(deadline: Deadline): Timestamp =
    now + deadline.timeLeft

  def now: Timestamp =
    ofEpochMilli(currentTimeMillis)

  final def currentTimeMillis = System.currentTimeMillis

  implicit val TimestampShow: Show[Timestamp] = _.show

  implicit final val jsonEncoder: Encoder[Timestamp] = implementation.jsonEncoder
  implicit final val jsonDecoder: Decoder[Timestamp] = implementation.jsonDecoder
  final val StringTimestampJsonEncoder: Encoder[Timestamp] =
    implementation.StringTimestampJsonEncoder

  trait Companion {
    final val StringTimestampJsonEncoder: Encoder[Timestamp] =
      o => Json.fromString(o.toIsoString)

    final val NumericTimestampJsonEncoder: Encoder[Timestamp] =
      o => Json.fromLong(o.toEpochMilli)

    implicit final val jsonEncoder: Encoder[Timestamp] = NumericTimestampJsonEncoder

    implicit final val jsonDecoder: Decoder[Timestamp] =
      cursor =>
        if (cursor.value.isString)
          cursor.as[String] map parse
        else
          cursor.as[Long] map ofEpochMilli

    def ofEpochMilli(o: Long): Timestamp

    def ofEpochSecond(o: Long): Timestamp =
      ofEpochMilli(o * 1000)

    def parse(string: String): Timestamp
  }
}
