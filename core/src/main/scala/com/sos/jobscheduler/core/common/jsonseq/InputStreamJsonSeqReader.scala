package com.sos.jobscheduler.core.common.jsonseq

import akka.util.ByteString
import com.google.common.base.Ascii.{LF, RS}
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.utils.UntilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader._
import io.circe.Json
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path

/**
  * MIME media type application/json-seq, RFC 7464 "JavaScript Object Notation (JSON) Text Sequences".
  * <p>
  *   This implementation depends on BufferedReader.readLine, which separates the input not only
  *   by LF but also by CR, CR/LF and EOF.
  * <p>
  *    Also, this class does not collapse consecutive RS.
  *
  * @author Joacim Zschimmer
  * @see https://tools.ietf.org/html/rfc7464
  */
class InputStreamJsonSeqReader(in: SeekableInputStream, blockSize: Int = BlockSize)
extends AutoCloseable {

  private val block = new Array[Byte](blockSize)
  private var blockPos = 0L
  private var blockLength = 0
  private var blockRead = 0
  private val byteStringBuilder = ByteString.newBuilder

  private var lineNumber: Long = 1  // -1 for unknown line number after seek
  lazy val iterator: Iterator[PositionAnd[Json]] = UntilNoneIterator(read)

  private var closed = false

  /** Closes underlying `SeekableInputStream`. */
  def close() = {
    closed = true
    in.close()
  }

  def isClosed = closed

  final def read(): Option[PositionAnd[Json]] = {
    val pos = position
    readByteString() map (o ⇒
      io.circe.parser.parse(o.decodeString(UTF_8)) match {
        case Left(failure) ⇒ throwCorrupt2(lineNumber - 1, pos + 1/*RS*/, failure.message.replace(" (line 1, ", " ("))
        case Right(json) ⇒ PositionAnd(pos, json)
      })
  }

  private def readByteString(): Option[ByteString] = {
    var rsReached = false
    var lfReached = false
    var eof = false
    while (!lfReached && (!eof || blockRead < blockLength)) {
      if (blockRead == blockLength) {
        eof = !fillByteBuffer()
      }
      if (!rsReached && blockRead < blockLength) {
        if (block(blockRead) != RS)
          throwCorrupt("Missing ASCII RS at start of JSON sequence record")
        blockRead += 1
        rsReached = true
      }
      val start = blockRead
      while (blockRead < blockLength && (block(blockRead) != LF || { lfReached = true; false })) { blockRead += 1 }
      byteStringBuilder.putBytes(block, start, blockRead - start)
      if (lfReached) {
        blockRead += 1
      }
    }
    if (rsReached && !lfReached) throwCorrupt("Missing ASCII LF at end of JSON sequence record")
    lfReached ? {
      if (lineNumber != -1) lineNumber += 1
      val result = byteStringBuilder.result()
      byteStringBuilder.clear()
      result
    }
  }

  private def fillByteBuffer(): Boolean = {
    blockPos = position
    blockRead = 0
    val length = check { in.read(block) }
    if (length == -1) {
      blockLength = 0
      false  // EOF
    } else {
      blockLength = length
      true
    }
  }

  final def seek(pos: Long): Unit =
    if (pos != position) {
      if (pos >= blockPos && pos <= blockPos + blockLength) {
        blockRead = (pos - blockPos).toInt  // May be == blockLength
      } else {
        check { in.seek(pos) }
        blockPos = pos
        blockLength = 0
        blockRead = 0
      }
      lineNumber = -1
    }

  final def position = blockPos + blockRead

  private def check[A](body: ⇒ A): A =
    try body
    catch { case _: IOException if closed ⇒ sys.error("JSON sequence file has been closed while reading") }

  private def throwCorrupt(extra: String) =
    throwCorrupt2(lineNumber, position, extra)
}

object InputStreamJsonSeqReader
{
  private val BlockSize = 4096

  def open(file: Path, blockSize: Int = BlockSize): InputStreamJsonSeqReader =
    new InputStreamJsonSeqReader(SeekableInputStream.openFile(file), blockSize)

  private def throwCorrupt2(lineNumber: Long, position: Long, extra: String) = {
    val where = if (lineNumber >= 0) s"line $lineNumber" else s"position $position"
    sys.error(s"JSON sequence is corrupt at $where: $extra")
  }
}
