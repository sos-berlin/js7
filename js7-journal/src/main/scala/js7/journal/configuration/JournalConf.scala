package js7.journal.configuration

import com.typesafe.config.Config
import js7.base.auth.UserId
import js7.base.configutils.Configs.*
import js7.base.convert.As.StringAsByteCountWithDecimalPrefix
import js7.base.log.Logger
import js7.base.time.JavaTimeConverters.*
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

final case class JournalConf(
  syncOnCommit: Boolean,
  simulateSync: Option[FiniteDuration],
  delay: FiniteDuration,
  coalesceEventLimit: Int,
  snapshotPeriod: FiniteDuration,
  snapshotSizeLimit: Long,
  snapshotSizeEstimateEventThreshold: Long,
  ackWarnDurations: Seq[FiniteDuration],
  persistWarnDurations: Seq[FiniteDuration],
  deleteObsoleteFiles: Boolean,
  releaseEventsUserIds: Set[UserId] = Set.empty,
  slowCheckState: Boolean = false,
  infoLogEvents: Set[String] = Set.empty)


object JournalConf:
  private val logger = Logger[this.type]
  private val slowCheckStateKey = "js7.journal.slow-check-state"

  def fromConfig(config: Config): JournalConf =
    val syncOnCommit = config.getBoolean("js7.journal.sync")
    val delay = config.getDuration("js7.journal.delay").toFiniteDuration
    lazy val syncDelay = config.getDuration("js7.journal.sync-delay").toFiniteDuration
    val slowCheckState = config.getBoolean(slowCheckStateKey)
    if slowCheckState then logger.info(s"Slowing down due to $slowCheckStateKey = true")
    new JournalConf(
      syncOnCommit = syncOnCommit,
      simulateSync = config.durationOption("js7.journal.simulate-sync").map(_.toFiniteDuration),
      delay = (if syncOnCommit then syncDelay max delay else delay) min 1.s,
      coalesceEventLimit = config.as[Int]("js7.journal.coalesce-event-limit"),  // TODO Limit byte count to avoid OutOfMemoryError?
      snapshotPeriod = config.getDuration("js7.journal.snapshot.period").toFiniteDuration,
      snapshotSizeLimit = config.as("js7.journal.snapshot.when-bigger-than")(StringAsByteCountWithDecimalPrefix),
      snapshotSizeEstimateEventThreshold = config.as(
        "js7.journal.snapshot.estimate-event-threshold")(StringAsByteCountWithDecimalPrefix),
      ackWarnDurations = config.getDurationList("js7.journal.log.ack-warn-durations")
        .asScala.toSeq.map(_.toFiniteDuration),
      persistWarnDurations = config.getDurationList("js7.journal.persist-warn-durations")
        .asScala.toSeq.map(_.toFiniteDuration),
      deleteObsoleteFiles = config.getBoolean("js7.journal.remove-obsolete-files"),
      releaseEventsUserIds = config.seqAs[UserId]("js7.journal.users-allowed-to-release-events").toSet,
      slowCheckState = slowCheckState,
      infoLogEvents = config.seqAs[String]("js7.journal.log.info-events").toSet)
