package js7.core.event.journal

import js7.base.auth.UserId
import js7.base.convert.As.StringAsByteCountWithDecimalPrefix
import js7.common.configutils.Configs._
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters._
import com.typesafe.config.Config
import scala.jdk.CollectionConverters._
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
  ackWarnDurations: Seq[FiniteDuration],
  deleteObsoleteFiles: Boolean,
  releaseEventsUserIds: Set[UserId] = Set.empty,
  slowCheckState: Boolean = false,
  useJournaledStateAsSnapshot: Boolean = false)

object JournalConf
{
  private val logger = Logger(getClass)
  private val checkStateKey = "js7.journal.slow-check-state"

  def fromConfig(config: Config) = {
    val syncOnCommit = config.getBoolean("js7.journal.sync")
    val delay = config.getDuration("js7.journal.delay").toFiniteDuration
    lazy val syncDelay = config.getDuration("js7.journal.sync-delay").toFiniteDuration
    val checkJournaledState = config.getBoolean(checkStateKey) || sys.props.contains("TEST")
    if (checkJournaledState) logger.warn(s"Slowing down due to $checkStateKey = true")
    new JournalConf(
      syncOnCommit = syncOnCommit,
      simulateSync = config.durationOption("js7.journal.simulate-sync") map (_.toFiniteDuration),
      delay = (if (syncOnCommit) syncDelay max delay else delay) min 1.second,
      eventLimit = config.as[Int]("js7.journal.event-buffer-size"),  // TODO Limit byte count to avoid OutOfMemoryError?
      snapshotPeriod = config.getDuration("js7.journal.snapshot.period").toFiniteDuration,
      snapshotSizeLimit = config.as("js7.journal.snapshot.when-bigger-than")(StringAsByteCountWithDecimalPrefix),
      snapshotLogProgressPeriod = config.getDuration("js7.journal.snapshot.log-period").toFiniteDuration,
      snapshotLogProgressActorLimit = config.getInt("js7.journal.snapshot.log-actor-limit"),
      ackWarnDurations = config.getDurationList("js7.journal.ack-warn-durations")
        .asScala.toSeq.map(_.toFiniteDuration),
      deleteObsoleteFiles = config.getBoolean("js7.journal.remove-obsolete-files"),
      releaseEventsUserIds = config.seqAs[UserId]("js7.journal.users-allowed-to-release-events").toSet,
      slowCheckState = checkJournaledState,
      useJournaledStateAsSnapshot = config.getBoolean("js7.journal.use-journaled-state-as-snapshot"))
  }
}
