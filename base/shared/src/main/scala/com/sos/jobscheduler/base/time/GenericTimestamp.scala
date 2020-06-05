package js7.base.time

import cats.Show
import js7.base.time.GenericTimestamp._
import io.circe
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
    final val StringTimestampJsonEncoder: circe.Encoder[A] =
      o => circe.Json.fromString(o.toIsoString)

    final val NumericTimestampJsonEncoder: circe.Encoder[A] =
      o => circe.Json.fromLong(o.toEpochMilli)

    implicit final val jsonEncoder: circe.Encoder[A] = NumericTimestampJsonEncoder

    implicit final val jsonDecoder: circe.Decoder[A] =
      cursor =>
        if (cursor.value.isNumber)
          cursor.as[Long] map ofEpochMilli
        else
          cursor.as[String] map parse

    final def apply(string: String): A =
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
