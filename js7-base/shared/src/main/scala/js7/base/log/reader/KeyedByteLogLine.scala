package js7.base.log.reader

import fs2.Chunk
import java.time.Instant
import js7.base.log.LogLevel

final case class KeyedByteLogLine(logLevel: LogLevel, fileInstant: Instant, posAndLine: PosAndLine):

  def logLineKey: LogLineKey =
    LogLineKey(logLevel, fileInstant, posAndLine.position)

  def byteLine: Chunk[Byte] =
    posAndLine.byteLine

  def lineAsString: String =
    posAndLine.lineAsString

  def toKeyedLogLine: KeyedLogLine =
    KeyedLogLine(logLineKey, posAndLine.lineAsString)
