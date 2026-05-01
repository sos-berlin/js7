package js7.base.log.reader

import fs2.Chunk
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence

final case class PosAndLine(position: Long, byteLine: Chunk[Byte]):

  lazy val lineAsString: String =
    byteLine.utf8String


object PosAndLine:

  def fromPair(pair: (Long, Chunk[Byte])): PosAndLine =
    PosAndLine(pair._1, pair._2)
