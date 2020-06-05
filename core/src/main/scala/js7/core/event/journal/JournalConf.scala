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
  private val checkStateKey = "jobscheduler.journal.slow-check-state"

  def fromConfig(config: Config) = {
    val syncOnCommit = config.getBoolean("jobscheduler.journal.sync")
    val delay = config.getDuration("jobscheduler.journal.delay").toFiniteDuration
    lazy val syncDelay = config.getDuration("jobscheduler.journal.sync-delay").toFiniteDuration
    val checkJournaledState = config.getBoolean(checkStateKey) || sys.props.contains("TEST")
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
      ackWarnDurations = config.getDurationList("jobscheduler.journal.ack-warn-durations")
        .asScala.toSeq.map(_.toFiniteDuration),
      deleteObsoleteFiles = config.getBoolean("jobscheduler.journal.remove-obsolete-files"),
      releaseEventsUserIds = config.seqAs[UserId]("jobscheduler.journal.users-allowed-to-release-events").toSet,
      slowCheckState = checkJournaledState,
      useJournaledStateAsSnapshot = config.getBoolean("jobscheduler.journal.use-journaled-state-as-snapshot"))
  }
}
