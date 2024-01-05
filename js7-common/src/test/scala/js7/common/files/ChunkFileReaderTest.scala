package js7.common.files

import cats.syntax.foldable.*
import cats.syntax.monoid.*
import js7.base.data.ByteSequence.ops.*
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.fs2utils.Fs2ChunkByteSequence.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.base.test.OurTestSuite
import js7.base.utils.AutoClosing.autoClosing
import js7.common.pekkoutils.ByteStringByteSequence.*
import org.apache.pekko.util.ByteString
import scala.collection.mutable
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class ChunkFileReaderTest extends OurTestSuite:

  "ChunkFileReader[ByteArray]" in:
    testWith[ByteArray]

  "ChunkFileReader[fs2.Chunk[Byte]" in:
    testWith[fs2.Chunk[Byte]]

  "ChunkFileReader[ByteString]" in:
    // Not used. Just for completeness. And test of Pekko's ByteString.
    testWith[ByteString]

  private def testWith[ByteSeq](using ByteSeq: ByteSequence[ByteSeq]) =
    withTemporaryFile("ChunkFileReaderTest", ".tmp"): file =>
      val content = ByteSeq.unsafeWrap(Random.nextBytes(3 * ChunkFileReader.ChunkSize - 7))
      file := content

      val result = mutable.Buffer.empty[ByteSeq]
      autoClosing(new ChunkFileReader[ByteSeq](file)): reader =>
        var eof = false
        while !eof do
          val byteSeq = reader.read()
          if byteSeq.isEmpty then
            eof = true
          else
            result += byteSeq

      assert(result.toSeq.combineAll == content)
