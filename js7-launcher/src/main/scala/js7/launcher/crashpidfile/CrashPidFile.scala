package js7.launcher.crashpidfile

import cats.effect.ResourceIO
import java.nio.ByteOrder
import java.nio.ByteOrder.BIG_ENDIAN
import java.nio.file.Path
import js7.base.io.process.Pid
import js7.launcher.crashpidfile.IndexedRecordSetImpl.Delete

type CrashPidFile = IndexedRecordSet[Pid]

object CrashPidFile:
  val Dummy: CrashPidFile =
    new CrashPidFile with IndexedRecordSet.Dummy[Pid]:
      override def toString = "CrashPidFile.Dummy"

  val byteOrder: ByteOrder = BIG_ENDIAN

type PidFileService = IndexedRecordSet[Pid]


object CrashPidFileService:

  /** File of binary encoded PIDs.
   * <p>
   *   Then file consists of 8 byte long records with big endian 64-bit integers.
   *   Deleted records are encoded as 0 (because there is no Pid(0)).
   * <p>
   *   Binary representation is chosen because multiple 8 byte long entries fits exactly in
   *   a file block.
   *   No record is stored in two consecutive blocks.
   *   Even in case of a disk problem (maybe disk is full),
   *   we always get completely written records, that means proper PIDs.
   * <p>
   *   The content of the file may be displayed with (Gnu) `od` and `awk`:
   *   <pre>
   *     echo -en '01234567\x00\x00\x00\x00\x0\0\0\x001234568' | \
   *     od -tu8 --endian=big -w8 | \
   *     awk '/&#x005e;[0-9a-f]+\s+([0-9]+) *$/{ if ($2 != 0) print $2 }'
   *   </pre>
   */
  def file(file: Path): ResourceIO[PidFileService] =
    IndexedRecordSet.file(file, 8/*Long*/, CrashPidFile.byteOrder, s"CrashPidFile:$file"):
      case (buf, _: Delete) => buf.putLong(0)
      case (buf, pid: Pid) => buf.putLong(pid.number)
