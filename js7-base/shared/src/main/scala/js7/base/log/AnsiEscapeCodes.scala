package js7.base.log

import fs2.Chunk
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import scala.util.matching.Regex

object AnsiEscapeCodes:

  /** Reset color and mode. */
  inline val resetColor = "\u001B[0m"
  inline val bold = "\u001B[1m"
  inline val black = "\u001B[30m"
  inline val red = "\u001B[31m"
  inline val green = "\u001B[32m"
  inline val blue = "\u001B[34m"
  inline val magenta = "\u001B[38;5;13m"
  inline val orange = "\u001B[38;5;9m"

  val HighlightRegex: Regex =
    """(?:\u001b\[[0-9;]*m)""".r

  def bold(string: String): String =
    highlight(bold)(string)

  def highlight(startSeq: String)(string: String): String =
    s"$startSeq$string$resetColor"

  def removeHighlights(line: fs2.Chunk[Byte]): Chunk[Byte] =
    if line.exists(_ == '\u001b') then
      Chunk.array:
        removeHighlights(line.utf8String).getBytes(UTF_8)
    else
      line

  // TODO Make it faster. Copy bytes directly, leave out \u001b...m
  def removeHighlights(line: String): String =
    HighlightRegex.replaceAllIn(line, "") // SLOW

  //extension (charSeq: CharSequence)
  //  def dropHighlightsAtStart: CharSequence =
  //    val length = charSeq.length
  //    var i = 0
  //    while i < length - 1 && charSeq.charAt(i) == '\u001b' && charSeq.charAt(i + 1) == '[' do
  //      i += 1
  //      while i < length && charSeq.charAt(i) != 'm' do
  //        i += 1
  //      if i == length then return charSeq // 'm' not found
  //      i += 1
  //    if i == 0 then
  //      charSeq
  //    else
  //      charSeq.subSequence(i, length)
