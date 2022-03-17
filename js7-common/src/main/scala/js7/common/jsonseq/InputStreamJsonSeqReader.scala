package js7.common.jsonseq

import cats.effect.Resource
import cats.instances.vector._
import cats.syntax.foldable._
import io.circe.Json
import java.io.IOException
import java.nio.file.Path
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops._
import js7.base.log.Logger
import js7.base.problem.{Problem, ProblemException}
import js7.base.utils.Ascii.{LF, RS}
import js7.base.utils.ScalaUtils.syntax._
import js7.common.jsonseq.InputStreamJsonSeqReader._
import js7.common.utils.UntilNoneIterator
import monix.eval.Task
import monix.execution.atomic.AtomicAny
import scala.collection.mutable

/**
  * MIME media type application/json-seq, RFC 7464 "JavaScript Object Notation (JSON) Text Sequences".
  * <p>
  *    This implementation class expects no LF in a JSON record
  *    and does not collapse consecutive RS.
  *
  * @author Joacim Zschimmer
  * @see https://tools.ietf.org/html/rfc7464
  * @see https://ndjson.org/
  */
final class InputStreamJsonSeqReader(
  inputStream_ : SeekableInputStream,
  name: String,
  blockSize: Int = BlockSize,
  withRS: Boolean = false)
extends AutoCloseable
{
  private val inAtomic = AtomicAny(inputStream_)
  private val block = new Array[Byte](blockSize)
  private var blockPos = 0L
  private var blockLength = 0
  private var blockRead = 0
  private val byteArrays = mutable.Buffer.empty[ByteArray]
  private var lineNumber: Long = 1  // -1 for unknown line number after seek
  lazy val iterator: Iterator[PositionAnd[Json]] = UntilNoneIterator(read())

  private def in = inAtomic.get() match {
    case null => throw new ClosedException(name)
    case o => o
  }

  /** Closes underlying `SeekableInputStream`. May be called asynchronously. */
  def close() =
    synchronized {
      for (in <- Option(inAtomic.getAndSet(null))) {
        in.close()
      }
    }

  private def isClosed = inAtomic.get() == null

  def read(): Option[PositionAnd[Json]] =
    readRaw().map(toJson)

  def toJson(positionAndRaw: PositionAnd[ByteArray]): PositionAnd[Json] = {
    val PositionAnd(pos, bytes) = positionAndRaw
    bytes.parseJson match {
      case Left(problem) =>
        val lineNr = lineNumber - 1
        val extra =
          if (problem.toString startsWith "JSON ParsingFailure: ")
            problem.toString.stripPrefix("JSON ParsingFailure: ").replace(" (line 1, ", " (")
          else
            problem.toString
        logger.warn(s"JSON sequence read from '$name' is corrupt at " +
          ((lineNr >= 0) ?? s"line $lineNr, ") +
          s"file position $pos: " +
          s"$extra: ${bytes.utf8StringTruncateAt(200)}")  // Do not expose JSON content with exception
        throwCorrupt2(lineNr, pos, extra)

      case Right(json) =>
        PositionAnd(pos, json)
    }
  }

  def readRaw(): Option[PositionAnd[ByteArray]] =
    synchronized {
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
        val chunk = ByteArray.unsafeWrap(java.util.Arrays.copyOfRange(block, start, blockRead + (if (lfReached) 1 else 0)))
        if (chunk.nonEmpty) {
          byteArrays += chunk
        }
        if (lfReached) {
          blockRead += 1
        }
      }
      if ((!withRS && byteArrays.nonEmpty || rsReached) && !lfReached) {
        logger.warn(s"Discarding truncated last record in '$name': ${byteArrays.toVector.combineAll.utf8String} (terminating LF is missing)")
        byteArrays.clear()
        seek(startPosition)  // Keep a proper file position at start of record
      }
      lfReached ? {
        if (lineNumber != -1) lineNumber += 1
        val result = ByteArray.combineAll(byteArrays)
        byteArrays.clear()
        PositionAnd(startPosition, result)
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
        logger.trace(s"seek $pos => blockPos=$pos blockRead=$blockRead blockLength=$blockLength")
      }
    }

  def position = blockPos + blockRead

  private def check[A](body: => A): A =
    try body
    catch { case _: IOException if isClosed => throw new ClosedException(name) }

  private def throwCorrupt(extra: String) =
    throwCorrupt2(lineNumber, position, extra)
}

object InputStreamJsonSeqReader
{
  private[jsonseq] val BlockSize = 8192
  private val logger = Logger(getClass)

  def resource(file: Path): Resource[Task, InputStreamJsonSeqReader] =
    Resource.fromAutoCloseable(Task(InputStreamJsonSeqReader.open(file)))

  def open(file: Path, blockSize: Int = BlockSize): InputStreamJsonSeqReader =
    new InputStreamJsonSeqReader(SeekableInputStream.openFile(file), name = file.getFileName.toString, blockSize)

  private def throwCorrupt2(lineNumber: Long, position: Long, extra: String) = {
    val where = if (lineNumber >= 0) s"line $lineNumber" else s"file position $position"
    sys.error(s"Read JSON sequence is corrupt at $where: $extra")
  }

  final class ClosedException private[InputStreamJsonSeqReader](file: String)
  extends ProblemException(JsonSeqFileClosedProblem(file))

  final case class JsonSeqFileClosedProblem(file: String) extends Problem.Coded {
    def arguments = Map("file" -> file)
  }
}
