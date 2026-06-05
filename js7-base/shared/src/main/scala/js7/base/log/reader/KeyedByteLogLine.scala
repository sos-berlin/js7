package js7.base.log.reader

import fs2.Chunk
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.LogLevel
import js7.base.problem.{Checked, Problem}
import js7.base.time.EpochNano.toEpochNano

final case class KeyedByteLogLine(logLevel: LogLevel, fileInstant: Instant, posAndLine: PosAndLine):

  def logLineKey: LogLineKey =
    LogLineKey(logLevel, fileInstant, posAndLine.position)

  def byteLine: Chunk[Byte] =
    posAndLine.byteLine

  def lineAsString: String =
    posAndLine.lineAsString

  def toKeyedLogLine: KeyedLogLine =
    KeyedLogLine(logLineKey, posAndLine.lineAsString)

  def asByteSeq: Chunk[Byte] =
    fs2.Chunk.array:
      s"$logLevel/${fileInstant.toEpochNano.toDecimalString}/${posAndLine.position} "
        .getBytes(UTF_8)
    ++ posAndLine.byteLine


object KeyedByteLogLine:

  def apply(key: LogLineKey, line: fs2.Chunk[Byte]): KeyedByteLogLine =
    new KeyedByteLogLine(key.logLevel, key.fileInstant, PosAndLine(key.position, line))

  def parse(chunk: fs2.Chunk[Byte]): Checked[KeyedByteLogLine] =
    chunk.vectorIndexOf(' ') match
      case -1 => Left(Problem("Invalid KeyedByteLogLine format"))
      case i =>
        LogLineKey.parse(chunk.take(i)).map: key =>
          KeyedByteLogLine(key, chunk.drop(i + 1))
