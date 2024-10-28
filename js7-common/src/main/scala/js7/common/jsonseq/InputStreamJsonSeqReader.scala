package js7.common.jsonseq

import cats.effect.{IO, Resource, ResourceIO}
import cats.instances.vector.*
import cats.syntax.foldable.*
import io.circe.Json
import java.io.IOException
import java.nio.file.Path
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.log.Logger
import js7.base.problem.{Problem, ProblemException}
import js7.base.utils.Ascii.{LF, RS}
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.InputStreamJsonSeqReader.*
import js7.common.utils.UntilNoneIterator
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
extends AutoCloseable:

  private val inAtomic = Atomic(inputStream_)
  private val block = new Array[Byte](blockSize)
  private var blockPos = 0L
  private var blockLength = 0
  private var blockRead = 0
  private val byteArrays = mutable.Buffer.empty[ByteArray]
  private var lineNumber: Long = 1  // -1 for unknown line number after seek
  lazy val iterator: Iterator[PositionAnd[Json]] = UntilNoneIterator(read())

  private def in = inAtomic.get() match
    case null => throw new ClosedException(name)
    case o => o

  /** Closes underlying `SeekableInputStream`. May be called asynchronously. */
  def close(): Unit =
    synchronized:
      for in <- Option(inAtomic.getAndSet(null)) do
        in.close()

  private def isClosed = inAtomic.get() == null

  def read(): Option[PositionAnd[Json]] =
    readRaw().map(toJson)

  def toJson(positionAndRaw: PositionAnd[ByteArray]): PositionAnd[Json] =
    val PositionAnd(pos, bytes) = positionAndRaw
    bytes.parseJson match
      case Left(problem) =>
        val lineNr = lineNumber - 1
        val extra =
          if problem.toString.startsWith("JSON ParsingFailure: ") then
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

  def readRaw(): Option[PositionAnd[ByteArray]] =
    synchronized:
      val startPosition = position
      var rsReached = false
      var lfReached = false
      var eof = false
      while !lfReached && (!eof || blockRead < blockLength) do
        if blockRead == blockLength then
          eof = !fillByteBuffer()
        if !rsReached && blockRead < blockLength then
          val byte = block(blockRead)
          if withRS then
            if byte != RS then
              throwCorrupt(f"Missing ASCII RS at start of JSON sequence record (instead read: $byte%02x)")
            blockRead += 1
            rsReached = true
        val start = blockRead
        while blockRead < blockLength && (block(blockRead) != LF || { lfReached = true; false }) do
          blockRead += 1
        val chunk = ByteArray.unsafeWrap(java.util.Arrays.copyOfRange(block, start, blockRead + (if lfReached then 1 else 0)))
        if chunk.nonEmpty then
          byteArrays += chunk
        if lfReached then
          blockRead += 1
      if (!withRS && byteArrays.nonEmpty || rsReached) && !lfReached then
        logger.warn(s"Discarding truncated last record in '$name': ${byteArrays.toVector.combineAll.utf8String} (terminating LF is missing)")
        byteArrays.clear()
        seek(startPosition)  // Keep a proper file position at start of record
      lfReached ? {
        if lineNumber != -1 then lineNumber += 1
        val result = ByteArray.combineAll(byteArrays)
        byteArrays.clear()
        PositionAnd(startPosition, result)
      }

  private def fillByteBuffer(): Boolean =
    blockPos = position
    blockRead = 0
    val length = check { in.read(block) }
    if length == -1 then
      blockLength = 0
      false  // EOF
    else
      blockLength = length
      true

  def seek(pos: Long): Unit =
    synchronized:
      if pos != position then
        if pos >= blockPos && pos <= blockPos + blockLength then
          blockRead = (pos - blockPos).toInt  // May be == blockLength
        else
          check { in.seek(pos) }
          blockPos = pos
          blockLength = 0
          blockRead = 0
        lineNumber = -1
        logger.trace(s"seek $pos => blockPos=$pos blockRead=$blockRead blockLength=$blockLength")

  def position: Long =
    blockPos + blockRead

  private def check[A](body: => A): A =
    try body
    catch { case _: IOException if isClosed => throw new ClosedException(name) }

  private def throwCorrupt(extra: String) =
    throwCorrupt2(lineNumber, position, extra)


object InputStreamJsonSeqReader:
  private[jsonseq] val BlockSize = 8192
  private val logger = Logger[this.type]

  def resource(file: Path): ResourceIO[InputStreamJsonSeqReader] =
    Resource.fromAutoCloseable(IO(InputStreamJsonSeqReader.open(file)))

  def open(file: Path, blockSize: Int = BlockSize): InputStreamJsonSeqReader =
    new InputStreamJsonSeqReader(SeekableInputStream.openFile(file), name = file.getFileName.toString, blockSize)

  private def throwCorrupt2(lineNumber: Long, position: Long, extra: String) =
    val where = if lineNumber >= 0 then s"line $lineNumber" else s"file position $position"
    sys.error(s"JSON sequence is corrupt at $where: $extra")

  final class ClosedException private[InputStreamJsonSeqReader](file: String)
  extends ProblemException(JsonSeqFileClosedProblem(file))

  final case class JsonSeqFileClosedProblem(file: String) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "file" -> file)
