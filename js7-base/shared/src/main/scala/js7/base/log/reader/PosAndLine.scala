package js7.base.log.reader

import fs2.Chunk
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence

final case class PosAndLine(position: Long, byteLine: Chunk[Byte]):

  lazy val lineAsString: String =
    byteLine.utf8String

  //def removeHighlights: PosAndLine =
  //  copy(byteLine = AnsiEscapeCodes.removeHighlights(byteLine))

  override def toString = s"PosAndLine($position:${byteLine.utf8StringTruncateAt(100)})"


object PosAndLine:

  def fromPair(pair: (Long, Chunk[Byte])): PosAndLine =
    PosAndLine(pair._1, pair._2)
