package js7.base.time

import cats.Show
import io.circe.{Decoder, Encoder, Json}
import js7.base.number.Numbers.{addSaturating, subtractSaturating}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
trait Timestamp extends Ordered[Timestamp]:

  def companion: Companion

  def toEpochMilli: Long

  def toEpochSecond: Long = toEpochMilli / 1000

  /** Returns an ISO-8601 string with milliseconds.
    * For example "2017-12-04T11:22:33.456Z".
    */
  def toIsoString: String

  def toTimeString: String =
    toIsoString.substring(11).stripSuffix("Z")

  final def isFiniteDurationCompatible: Boolean =
    toEpochMilli >= 0 && toEpochSecond < Long.MaxValue / 1_000_000

  final def checkFiniteDurationCompatible: Checked[this.type] =
    if !isFiniteDurationCompatible then
      Left(Problem(s"$toString is out of range"))
    else
      Right(this)

  def format(format: String, maybeTimezone: Option[String] = None): Checked[String]

  //Problem with sbt: def toIsoStringBuilder: StringBuilder

  def compare(o: Timestamp): Int = toEpochMilli compare o.toEpochMilli

  def +(o: FiniteDuration): Timestamp =
    copy(epochMilli = addSaturating(toEpochMilli, o.toMillis))

  def -(o: FiniteDuration): Timestamp =
    copy(epochMilli = subtractSaturating(toEpochMilli, o.toMillis))

  def -(o: Timestamp): FiniteDuration =
    new FiniteDuration(subtractSaturating(toEpochMilli, o.toEpochMilli), MILLISECONDS)

  infix final def min(o: Timestamp): Timestamp = if this < o then this else o

  infix final def max(o: Timestamp): Timestamp = if this > o then this else o

  final def roundToNextSecond: Timestamp = copy((toEpochMilli + 999)/ 1000 * 1000)

  final def secondsOnly: Timestamp = copy(toEpochMilli / 1000 * 1000)

  def roundTo(duration: FiniteDuration): Timestamp = (this + duration / 2).roundDownTo(duration)

  def roundDownTo(duration: FiniteDuration): Timestamp =
    val durationMillis = duration.toMillis
    companion.ofEpochMilli(toEpochMilli / durationMillis * durationMillis)

  def copy(epochMilli: Long): Timestamp

  def show: String = toString

  def pretty: String = toString.replace('T', ' ')

  override def toString: String =
    if toEpochMilli == 0 then "Epoch" else toIsoString


object Timestamp:
  private[time] val implementation: Companion = SystemTimestamp
  val Epoch: Timestamp = ofEpochMilli(0)
  val MaxValue: Timestamp = Epoch + FiniteDuration.MaxValue
  val Epsilon: FiniteDuration = 1.ms

  // TODO Return Checked
  def ofEpochSecond(second: Long): Timestamp =
    ofEpochMilli(second * 1000)

  def ofEpochMilli(milli: Long): Timestamp =
    implementation.ofEpochMilli(milli)

  def apply(dateTime: String): Timestamp =
    implementation.parse(dateTime)

  def checked(dateTime: String): Checked[Timestamp] =
    try
      Right(parse(dateTime))
    catch case NonFatal(t) =>
      Left(Problem(t.toStringWithCauses))

  def parse(dateTime: String): Timestamp =
    implementation.parse(dateTime)

  def now: Timestamp =
    ofEpochMilli(currentTimeMillis)

  final def currentTimeMillis: Long = System.currentTimeMillis

  implicit val TimestampShow: Show[Timestamp] = _.show

  implicit final val jsonEncoder: Encoder[Timestamp] = implementation.jsonEncoder
  implicit final val jsonDecoder: Decoder[Timestamp] = implementation.jsonDecoder
  final val StringTimestampJsonEncoder: Encoder[Timestamp] =
    implementation.StringTimestampJsonEncoder

  trait Companion:
    final val StringTimestampJsonEncoder: Encoder[Timestamp] =
      o => Json.fromString(o.toIsoString)

    final val NumericTimestampJsonEncoder: Encoder[Timestamp] =
      o => Json.fromLong(o.toEpochMilli)

    implicit final val jsonEncoder: Encoder[Timestamp] = NumericTimestampJsonEncoder

    implicit final val jsonDecoder: Decoder[Timestamp] =
      cursor =>
        if cursor.value.isString then
          cursor.as[String] map parse
        else
          cursor.as[Long] map ofEpochMilli

    def ofEpochMilli(o: Long): Timestamp

    def ofEpochSecond(o: Long): Timestamp =
      ofEpochMilli(o * 1000)

    def parse(string: String): Timestamp
