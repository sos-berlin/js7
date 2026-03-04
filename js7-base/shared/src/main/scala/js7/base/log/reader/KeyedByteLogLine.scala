package js7.base.log.reader

import fs2.Chunk
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence

final case class KeyedByteLogLine(key: LogLineKey, byteLine: Chunk[Byte]):
  def toKeyedLogLine: KeyedLogLine =
    KeyedLogLine(key, byteLine.utf8String)
