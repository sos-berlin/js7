package js7.data.event

import js7.base.BuildInfo
import js7.base.time.Timestamp
import js7.data.event.JournalHeader.Version
import scala.concurrent.duration.{Duration, FiniteDuration}

object JournalHeaders
{
  implicit final class RichJournalHeader(private val self: JournalHeader) extends AnyVal
  {
    def nextGeneration(
      eventId: EventId,
      totalEventCount: Long,
      totalRunningTime: FiniteDuration,
      timestamp: Timestamp = Timestamp.now)
  : JournalHeader =
      self.copy(
        eventId = eventId,
        generation = self.generation + 1,
        totalEventCount = totalEventCount,
        totalRunningTime = totalRunningTime,
        timestamp = timestamp,
        version = Version,
        js7Version = BuildInfo.longVersion,
        buildId = BuildInfo.buildId)
  }

  def forTest(journalId: JournalId, eventId: EventId = EventId.BeforeFirst): JournalHeader =
    new JournalHeader(
      journalId,
      eventId = eventId,
      generation = 1,
      totalEventCount = 0,
      Duration.Zero,
      timestamp = Timestamp.now,
      startedAt = Timestamp.now,
      js7Version = BuildInfo.longVersion,
      version = Version,
      buildId = BuildInfo.buildId)

  def initial(journalId: JournalId) =
    new JournalHeader(
      journalId,
      eventId = EventId.BeforeFirst,
      generation = 0,
      totalEventCount = 0,
      Duration.Zero,
      timestamp = Timestamp.now,
      startedAt = Timestamp.now,
      version = Version,
      js7Version = BuildInfo.longVersion,
      buildId = BuildInfo.buildId)
}
