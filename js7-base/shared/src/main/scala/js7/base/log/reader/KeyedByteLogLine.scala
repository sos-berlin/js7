package js7.base.log.reader

import fs2.Chunk
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.problem.{Checked, Problem}
import js7.base.time.EpochNano.toEpochNano

/** An efficient representation of a log line with a LogLineKey.
  *
  * The line is a `Chunk[Byte]`, which may be converted to a String via `lineAsString`.
  * Then, a KeyedByteLogLine contains both the byte and the String representation of the line. */
final case class KeyedByteLogLine(fileInstant: Instant, posAndLine: PosAndLine):

  def logLineKey: LogLineKey =
    LogLineKey(fileInstant, posAndLine.position)

  def byteLine: Chunk[Byte] =
    posAndLine.byteLine

  def lineAsString: String =
    posAndLine.lineAsString

  def toKeyedLogLine: KeyedLogLine =
    KeyedLogLine(logLineKey, posAndLine.lineAsString)

  //def removeHighlights: KeyedByteLogLine =
  //  copy(posAndLine = posAndLine.removeHighlights)

  def asByteSeq: Chunk[Byte] =
    fs2.Chunk.array:
      s"${fileInstant.toEpochNano.toDecimalString}/${posAndLine.position} ".getBytes(UTF_8)
    ++ posAndLine.byteLine


object KeyedByteLogLine:

  def apply(key: LogLineKey, line: fs2.Chunk[Byte]): KeyedByteLogLine =
    new KeyedByteLogLine(key.fileInstant, PosAndLine(key.position, line))

  def parse(chunk: fs2.Chunk[Byte]): Checked[KeyedByteLogLine] =
    chunk.vectorIndexOf(' ') match
      case -1 => Left(Problem("Invalid KeyedByteLogLine format"))
      case i =>
        LogLineKey.parse(chunk.take(i)).map: key =>
          KeyedByteLogLine(key, chunk.drop(i + 1))
