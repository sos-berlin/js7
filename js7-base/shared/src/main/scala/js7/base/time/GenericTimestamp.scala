package js7.base.time

import cats.Show
import io.circe.{Decoder, Encoder, Json}
import js7.base.problem.Checked
import js7.base.time.GenericTimestamp._
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait GenericTimestamp[A <: GenericTimestamp[A]] extends Ordered[A] {
  this: A =>

  def companion: Companion[A]

  def toEpochMilli: Long

  def toEpochSecond = toEpochMilli / 1000

  /** Returns an ISO-8601 string with milliseconds.
    * For example "2017-12-04T11:22:33.456Z".
    */
  def toIsoString: String

  def format(format: String, maybeTimezone: Option[String] = None): Checked[String]

  //Problem with sbt: def toIsoStringBuilder: StringBuilder

  def compare(o: A) = toEpochMilli compare o.toEpochMilli

  def +(o: FiniteDuration): A =
    copy(epochMilli = toEpochMilli + o.toMillis)

  def -(o: FiniteDuration): A =
    copy(epochMilli = toEpochMilli - o.toMillis)

  def -(o: A): FiniteDuration =
    new FiniteDuration(toEpochMilli - o.toEpochMilli, MILLISECONDS)

  final def min(o: A) = if (this < o) this else o

  final def max(o: A) = if (this > o) this else o

  final def roundToNextSecond: A = copy((toEpochMilli + 999)/ 1000 * 1000)

  final def secondsOnly: A = copy(toEpochMilli / 1000 * 1000)

  def roundTo(duration: FiniteDuration): A = this + duration / 2 roundDownTo duration

  def roundDownTo(duration: FiniteDuration): A = {
    val durationMillis = duration.toMillis
    companion.ofEpochMilli(toEpochMilli / durationMillis * durationMillis)
  }

  def copy(epochMilli: Long): A

  def show = toString

  def pretty = toString.replace('T', ' ')

  override def toString = toIsoString
}

object GenericTimestamp {
  trait Companion[A <: GenericTimestamp[A]] {
    final val Epoch = ofEpochMilli(0)
    final val StringTimestampJsonEncoder: Encoder[A] =
      o => Json.fromString(o.toIsoString)

    final val NumericTimestampJsonEncoder: Encoder[A] =
      o => Json.fromLong(o.toEpochMilli)

    implicit final val jsonEncoder: Encoder[A] = NumericTimestampJsonEncoder

    implicit final val jsonDecoder: Decoder[A] =
      cursor =>
        if (cursor.value.isString)
          cursor.as[String] map parse
        else
          cursor.as[Long] map ofEpochMilli

    def apply(string: String): A =
      parse(string)

    def ofEpochMilli(o: Long): A

    def ofEpochSecond(o: Long): A =
      ofEpochMilli(o * 1000)

    def ofDeadline(deadline: Deadline): A =
      now + deadline.timeLeft

    def parse(string: String): A

    final def now: A = ofEpochMilli(currentTimeMillis)

    final def currentTimeMillis = System.currentTimeMillis

    implicit val TimestampShow: Show[A] = _.show
  }
}
