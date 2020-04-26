package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.convert.As.StringAsByteCountWithDecimalPrefix
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.typesafe.config.Config
import scala.concurrent.duration._

final case class JournalConf(
  syncOnCommit: Boolean,
  simulateSync: Option[FiniteDuration],
  delay: FiniteDuration,
  eventLimit: Int,
  snapshotPeriod: FiniteDuration,
  snapshotSizeLimit: Long,
  snapshotLogProgressPeriod: FiniteDuration,
  snapshotLogProgressActorLimit: Int,
  ackWarnDuration: FiniteDuration,
  deleteObsoleteFiles: Boolean,
  releaseEventsUserIds: Set[UserId] = Set.empty,
  slowCheckJournaledState: Boolean = false)

object JournalConf
{
  private val logger = Logger(getClass)
  private val checkStateKey = "jobscheduler.journal.slow-check-journaled-state"

  def fromConfig(config: Config) = {
    val syncOnCommit = config.getBoolean("jobscheduler.journal.sync")
    val delay = config.getDuration("jobscheduler.journal.delay").toFiniteDuration
    lazy val syncDelay = config.getDuration("jobscheduler.journal.sync-delay").toFiniteDuration
    val checkJournaledState = config.getBoolean(checkStateKey)
    if (checkJournaledState) logger.warn(s"Slowing down due to $checkStateKey = true")
    new JournalConf(
      syncOnCommit = syncOnCommit,
      simulateSync = config.durationOption("jobscheduler.journal.simulate-sync") map (_.toFiniteDuration),
      delay = (if (syncOnCommit) syncDelay max delay else delay) min 1.second,
      eventLimit = config.as[Int]("jobscheduler.journal.event-buffer-size"),  // TODO Limit byte count to avoid OutOfMemoryError?
      snapshotPeriod = config.getDuration("jobscheduler.journal.snapshot.period").toFiniteDuration,
      snapshotSizeLimit = config.as("jobscheduler.journal.snapshot.when-bigger-than")(StringAsByteCountWithDecimalPrefix),
      snapshotLogProgressPeriod = config.getDuration("jobscheduler.journal.snapshot.log-period").toFiniteDuration,
      snapshotLogProgressActorLimit = config.getInt("jobscheduler.journal.snapshot.log-actor-limit"),
      ackWarnDuration = config.getDuration("jobscheduler.journal.ack-warn-duration").toFiniteDuration,
      deleteObsoleteFiles = config.getBoolean("jobscheduler.journal.remove-obsolete-files"),
      releaseEventsUserIds = config.seqAs[UserId]("jobscheduler.journal.users-allowed-to-release-events").toSet,
      slowCheckJournaledState = checkJournaledState)
  }
}
