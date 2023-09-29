package js7.data.event

import js7.base.BuildInfo
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.event.JournalHeader.Version
import scala.concurrent.duration.FiniteDuration

object JournalHeaders
{
  implicit final class RichJournalHeader(private val self: JournalHeader) extends AnyVal
  {
    def nextGeneration[S <: BasicState[S]](
      eventId: EventId,
      totalEventCount: Long,
      totalRunningTime: FiniteDuration,
      timestamp: Timestamp = Timestamp.now)
      (implicit S: BasicState.Companion[S])
    : JournalHeader =
      self.copy(
        typeName = Some(S.name),
        eventId = eventId,
        generation = self.generation + 1,
        totalEventCount = totalEventCount,
        totalRunningTime = totalRunningTime,
        timestamp = timestamp,
        version = Version,
        js7Version = BuildInfo.longVersion,
        buildId = BuildInfo.buildId)
  }

  def forTest(typeName: String, journalId: JournalId, eventId: EventId = EventId.BeforeFirst): JournalHeader =
    new JournalHeader(
      typeName = Some(typeName),
      journalId,
      eventId = eventId,
      generation = 1,
      totalEventCount = 0,
      ZeroDuration,
      timestamp = Timestamp.now,
      initiallyStartedAt = Timestamp.now,
      js7Version = BuildInfo.longVersion,
      version = Version,
      buildId = BuildInfo.buildId)

  def initial[S <: BasicState[S]](journalId: JournalId)(implicit S: BasicState.Companion[S])
  : JournalHeader =
    new JournalHeader(
      typeName = Some(S.name),
      journalId,
      eventId = EventId.BeforeFirst,
      generation = 0,
      totalEventCount = 0,
      ZeroDuration,
      timestamp = Timestamp.now,
      initiallyStartedAt = Timestamp.now,
      version = Version,
      js7Version = BuildInfo.longVersion,
      buildId = BuildInfo.buildId)
}
