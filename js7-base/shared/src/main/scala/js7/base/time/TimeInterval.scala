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


type NonEmptyTimeInterval = TimeInterval.Standard | TimeInterval.Always

object TimeInterval:
  val never: TimeInterval = TimeInterval(Timestamp.ofEpochMilli(Long.MinValue), 0.s)
  val alwaysSinceEpoch: TimeInterval = TimeInterval(Timestamp.Epoch, FiniteDuration.MaxValue)

  def apply(start: Timestamp, duration: FiniteDuration): TimeInterval =
    Standard(start, duration)

  implicit def fromStartAndEnd(interval: (Timestamp, Timestamp)): TimeInterval =
    TimeInterval(interval._1, interval._2 - interval._1)


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

    override def toString = s"TimeInterval($start, ${duration.pretty})"

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
