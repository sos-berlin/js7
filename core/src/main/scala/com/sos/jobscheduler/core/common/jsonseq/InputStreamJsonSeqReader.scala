package com.sos.jobscheduler.core.common.jsonseq

import akka.util.ByteString
import com.google.common.base.Ascii.{LF, RS}
import com.sos.jobscheduler.base.problem.ProblemException
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.UntilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader._
import com.sos.jobscheduler.core.problems.JsonSeqFileClosedProblem
import io.circe.Json
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import monix.execution.atomic.AtomicAny

/**
  * MIME media type application/json-seq, RFC 7464 "JavaScript Object Notation (JSON) Text Sequences".
  * <p>
  *    This implementation class expects no LF in a JSON record
  *    and does not collapse consecutive RS.
  *
  * @author Joacim Zschimmer
  * @see https://tools.ietf.org/html/rfc7464
  * @see http://ndjson.org/
  */
final class InputStreamJsonSeqReader(inputStream_ : SeekableInputStream, name: String, blockSize: Int = BlockSize, withRS: Boolean = false)
extends AutoCloseable {

  private val inAtomic = AtomicAny(inputStream_)
  private val block = new Array[Byte](blockSize)
  private var blockPos = 0L
  private var blockLength = 0
  private var blockRead = 0
  private val byteStringBuilder = ByteString.newBuilder
  private var lineNumber: Long = 1  // -1 for unknown line number after seek
  lazy val iterator: Iterator[PositionAnd[Json]] = UntilNoneIterator(read)

  private def in = inAtomic.get match {
    case null ⇒ throw new ClosedException(name)
    case o ⇒ o
  }

  /** Closes underlying `SeekableInputStream`. May be called asynchronously. */
  def close() =
    synchronized {
      for (in ← Option(inAtomic.getAndSet(null))) {
        in.close()
      }
    }

  private def isClosed = inAtomic.get == null

  def read(): Option[PositionAnd[Json]] =
    synchronized {
      val pos = position
      readByteString() map (o ⇒
        io.circe.parser.parse(o.decodeString(UTF_8)) match {
          case Left(failure) ⇒ throwCorrupt2(lineNumber - 1, pos, failure.message.replace(" (line 1, ", " ("))
          case Right(json) ⇒ PositionAnd(pos, json)
        })
    }

  private def readByteString(): Option[ByteString] = {
    val startPosition = position
    var rsReached = false
    var lfReached = false
    var eof = false
    while (!lfReached && (!eof || blockRead < blockLength)) {
      if (blockRead == blockLength) {
        eof = !fillByteBuffer()
      }
      if (!rsReached && blockRead < blockLength) {
        val byte = block(blockRead)
        if (withRS) {
          if (byte != RS)
            throwCorrupt(f"Missing ASCII RS at start of JSON sequence record (instead read: $byte%02x)")
          blockRead += 1
          rsReached = true
        }
      }
      val start = blockRead
      while (blockRead < blockLength && (block(blockRead) != LF || { lfReached = true; false })) { blockRead += 1 }
      byteStringBuilder.putBytes(block, start, blockRead - start)
      if (lfReached) {
        blockRead += 1
      }
    }
    if ((!withRS && byteStringBuilder.nonEmpty || rsReached) && !lfReached) {
      logger.warn(s"Discarding truncated last record in '$name': ${byteStringBuilder.result().utf8String} (terminating LF is missing)")
      byteStringBuilder.clear()
      seek(startPosition)  // Keep a proper file position at start of record
    }
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

  def seek(pos: Long): Unit =
    synchronized {
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
    }

  def position = blockPos + blockRead

  private def check[A](body: ⇒ A): A =
    try body
    catch { case _: IOException if isClosed ⇒ throw new ClosedException(name) }

  private def throwCorrupt(extra: String) =
    throwCorrupt2(lineNumber, position, extra)
}

object InputStreamJsonSeqReader
{
  private[jsonseq] val BlockSize = 4096
  private val logger = Logger(getClass)

  def open(file: Path, blockSize: Int = BlockSize): InputStreamJsonSeqReader =
    new InputStreamJsonSeqReader(SeekableInputStream.openFile(file), name = file.getFileName.toString, blockSize)

  private def throwCorrupt2(lineNumber: Long, position: Long, extra: String) = {
    val where = if (lineNumber >= 0) s"line $lineNumber" else s"position $position"
    sys.error(s"JSON sequence is corrupt at $where: $extra")
  }

  final class ClosedException private[InputStreamJsonSeqReader](file: String)
    extends ProblemException(JsonSeqFileClosedProblem(file))
}
