package js7.base.time

import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import scala.concurrent.duration.*
import scala.language.implicitConversions

sealed trait TimeInterval:
  def start: Timestamp
  def duration: FiniteDuration

  def end: Timestamp

  def contains(timestamp: Timestamp): Boolean

  def startsBefore(timestamp: Timestamp): Boolean

  def endsBefore(timestamp: Timestamp): Boolean

  inline final def tryCombine(other: TimeInterval): Option[TimeInterval] =
    TimeInterval.tryCombine(this, other)


type NonEmptyTimeInterval = TimeInterval.Standard | TimeInterval.Always

object TimeInterval:

  def apply(start: Timestamp, duration: FiniteDuration): TimeInterval =
    Standard(start, duration)

  implicit def fromStartAndEnd(interval: (Timestamp, Timestamp)): TimeInterval =
    TimeInterval(interval._1, interval._2 - interval._1)

  def tryCombine(a: TimeInterval, b: TimeInterval): Option[TimeInterval] =
    (a, b) match
      case (a: Standard, b: Standard) => a.tryCombineStandard(b)
      case (Always, _) => Some(Always)
      case (_, Always) => Some(Always)
      case (Never, _) => None
      case (_, Never) => None


  final case class Standard(start: Timestamp, duration: FiniteDuration)
  extends TimeInterval:
    assertThat(!duration.isNegative)

    def end: Timestamp = start + duration

    def contains(timestamp: Timestamp): Boolean =
      start <= timestamp && !endsBefore(timestamp)

    def startsBefore(timestamp: Timestamp): Boolean =
      start <= timestamp

    def endsBefore(timestamp: Timestamp): Boolean =
      end <= timestamp

    inline def tryCombine(that: Standard): Option[Standard] =
      tryCombineStandard(that)

    private[TimeInterval] def tryCombineStandard(that: Standard): Option[Standard] =
      Standard.tryCombine2(this, that) orElse Standard.tryCombine2(that, this)

    override def toString = s"TimeInterval($start, ${duration.pretty})"

  object Standard:
    private def tryCombine2(a: Standard, b: Standard): Option[Standard] =
      if a.end == b.start then
        Some(Standard(a.start, duration = b.end - a.start))
      else if a.contains(b.start) then
        Some(Standard(a.start, duration = a.end.max(b.end) - a.start))
      else
        None


  type Never = Never.type
  object Never extends TimeInterval:
    val start: Timestamp = Timestamp.Epoch
    val end: Timestamp = Timestamp.Epoch
    val duration: FiniteDuration = Duration.Zero

    def contains(timestamp: Timestamp) =
      false

    def startsBefore(timestamp: Timestamp) =
      false

    def endsBefore(timestamp: Timestamp) =
      true

    override def toString = "Never"


  // TODO Only used for AdmissionTimeSwitcher. Move Always to AdmissionTimeSwitcher!
  type Always = Always.type
  object Always extends TimeInterval:
    val start: Timestamp = Timestamp.Epoch
    val end: Timestamp = Timestamp.MaxValue
    val duration: FiniteDuration = end - start

    def contains(timestamp: Timestamp) =
      true

    def startsBefore(timestamp: Timestamp) =
      true

    def endsBefore(timestamp: Timestamp) =
      false

    override def toString = "Always"
