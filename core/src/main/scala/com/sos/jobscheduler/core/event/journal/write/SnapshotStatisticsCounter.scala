package com.sos.jobscheduler.core.event.journal.write

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalazStyle._

/**
  * @author Joacim Zschimmer
  */
private[journal] final class SnapshotStatisticsCounter extends StatisticsCounter
{
  private var snapshots = 0

  def countSnapshot(): Unit =
    snapshots += 1

  override def toString =
    if (snapshots == 0) "(no snapshots)"
    else s"$snapshots snapshots" //+ (if (syncs > 0) s", $syncs syncs" else "")

  def debugString: Option[String] =
    (snapshots > 0 && stopwatch.duration >= 1.s) ? timingString

  protected def timingString =
    stopwatch.itemsPerSecondString(snapshots, "snapshot objects") + " written"
}
