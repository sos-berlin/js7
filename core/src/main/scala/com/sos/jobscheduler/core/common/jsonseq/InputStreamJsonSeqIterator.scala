package com.sos.jobscheduler.core.common.jsonseq

import akka.util.ByteString
import com.google.common.base.Ascii.{LF, RS}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqIterator._
import io.circe.Json
import java.io.InputStream
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.AbstractIterator
import scala.util.Try

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
final class InputStreamJsonSeqIterator(in: InputStream, blockSize: Int = BlockSize, charBufferSize: Int = CharBufferSize)
extends AbstractIterator[PositionAnd[Json]] {

  private val block = new Array[Byte](blockSize)
  private var blockLength = 0
  private var blockRead = 0
  val byteStringBuilder = ByteString.newBuilder

  private var lineNumber = 0
  private var filePosition = 0L
  private var eof = false

  private var nextRecord: Try[Option[PositionAnd[Json]]] =
    readNext()

  def hasNext = nextRecord.get.nonEmpty

  def next() = {
    val result = nextRecord.get getOrElse { throw new NoSuchElementException("InputStreamJsonSeqIterator") }
    nextRecord = readNext()
    result
  }

  private def readNext(): Try[Option[PositionAnd[Json]]] = {
    val pos = filePosition
    Try { readByteString() map (o ⇒ PositionAnd(pos, o.decodeString(UTF_8).parseJson)) }
  }

  private def readByteString(): Option[ByteString] = {
    lineNumber += 1
    var rsReached = false
    var lfReached = false
    while (!lfReached && (!eof || blockRead < blockLength)) {
      if (blockRead == blockLength) {
        fillByteBuffer()
      }
      if (!rsReached && blockRead < blockLength) {
        if (block(blockRead) != RS)
          throwCorrupted(s"Missing ASCII RS at start of JSON sequence record at file position $filePosition")
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
    if (rsReached && !lfReached) throwCorrupted(s"Missing ASCII LF at end of JSON sequence record at file position $filePosition")
    lfReached ? {
      val result = byteStringBuilder.result()
      filePosition += byteStringBuilder.length + 2
      byteStringBuilder.clear()
      result
    }
  }

  private def fillByteBuffer(): Unit = {
    blockRead = 0
    val length = in.read(block.array)
    if (length == -1) {
      eof = true
      blockLength = 0
    } else {
      blockLength = length
    }
  }

  private def throwCorrupted(extra: String) = sys.error(s"JSON sequence is corrupted at line $lineNumber. $extra")
}

object InputStreamJsonSeqIterator {
  private val BlockSize = 4096
  private val CharBufferSize = 1000
}
