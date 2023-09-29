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

object TimeInterval:
  val never = TimeInterval(Timestamp.ofEpochMilli(Long.MinValue), 0.s)
  val alwaysSinceEpoch = TimeInterval(Timestamp.Epoch, FiniteDuration.MaxValue)

  def apply(start: Timestamp, duration: FiniteDuration): TimeInterval =
    Standard(start, duration)

  implicit def fromStartAndEnd(interval: (Timestamp, Timestamp)): TimeInterval =
    TimeInterval(interval._1, interval._2 - interval._1)

  final case class Standard(start: Timestamp, duration: FiniteDuration)
  extends TimeInterval:
    assertThat(!duration.isNegative)

    def end = start + duration

    def contains(timestamp: Timestamp): Boolean =
      start <= timestamp && !endsBefore(timestamp)

    def startsBefore(timestamp: Timestamp) =
      start <= timestamp

    def endsBefore(timestamp: Timestamp) =
      end <= timestamp

    override def toString = s"TimeInterval($start, ${duration.pretty})"

  object Never extends TimeInterval:
    val start = Timestamp.Epoch
    val end = Timestamp.Epoch
    val duration = Duration.Zero

    def contains(timestamp: Timestamp) =
      false

    def startsBefore(timestamp: Timestamp) =
      false

    def endsBefore(timestamp: Timestamp) =
      true

    override def toString = "Never"

  object Always extends TimeInterval:
    val start = Timestamp.Epoch
    val end = Timestamp.MaxValue
    val duration = end - start

    def contains(timestamp: Timestamp) =
      true

    def startsBefore(timestamp: Timestamp) =
      true

    def endsBefore(timestamp: Timestamp) =
      false

    override def toString = "Always"
