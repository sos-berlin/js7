package js7.journal.write

import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.ScalaUtils.syntax.*

/**
  * @author Joacim Zschimmer
  */
private[journal] final class SnapshotStatisticsCounter extends StatisticsCounter:
  private var snapshots = 0
  private var _fileLength = 0L

  def countSnapshot(): Unit =
    snapshots += 1

  def setFileLength(fileLength: Long): Unit =
    _fileLength = fileLength

  override def toString =
    if snapshots == 0 then "(no snapshot objects)"
    else s"$snapshots snapshot objects" //+ (if (syncs > 0) s", $syncs syncs" else "")

  def debugString: Option[String] =
    (snapshots > 0 && stopwatch.duration >= 3.s) ? timingString

  protected def timingString =
    val duration = stopwatch.duration
    itemsPerSecondString(duration, snapshots, "snapshot objects") + ", " +
      bytesPerSecondString(duration, _fileLength) + " written"
