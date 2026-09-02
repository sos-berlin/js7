package js7.tests.log

import cats.effect.{IO, Resource}
import cats.syntax.foldable.*
import fs2.Chunk
import java.io.FileInputStream
import js7.base.config.Js7Config
import js7.base.data.ByteSequence.ops.toAllByteSequenceOps
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.stringAsUtf8
import js7.base.io.OpaquePos
import js7.base.io.file.FileUtils.temporaryFileResource
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.Logger
import js7.base.log.reader.LogIndexConf
import js7.base.log.reader.recompressors.Recompressor
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.service.lz4.Lz4Recompressor
import js7.tests.log.RecompressorTest.*
import org.scalatest.Assertion
import scala.collection.mutable

final class RecompressorTest extends OurAsyncTestSuite:

  private given LogIndexConf = LogIndexConf.fromConfig(Js7Config.defaultConfig).orThrow

  "Recompressor" in:
    val recompressors = Recompressor.knownRecompressors :+ new Lz4Recompressor
    recompressors.foldMap: recompressor =>
      testRecompressor(recompressor)

  private def testRecompressor(recompressor: Recompressor): IO[Assertion] =
    IO.defer:
      logger.info(bold(s"$recompressor"))
      temporaryFileResource[IO]("RecompressorTest-").use: file =>
        val lines = Seq("LINE ONE\n", "LINE TWO +++++++++\n", "LINE THREE ---\n")
          .map(Chunk.stringAsUtf8)
        recompressor.toLogWriter(file).use: logWriter =>
          IO:
            val posAndLines = mutable.Buffer[(OpaquePos, Chunk[Byte])]()
            lines.foreach: line =>
              posAndLines += logWriter.markOpaquePos() -> line
              logWriter.write(line)
            //posAndLines += logWriter.markOpaquePos() -> Chunk.empty
            posAndLines.toVector
        .flatMap: posAndLines =>
          Resource.fromAutoCloseable(IO(FileInputStream(file.toFile))).use: recompressedIn =>
            IO:
              val in = recompressor.decompressingInputStream(recompressedIn)
              assert(Chunk.array(in.readAllBytes()) == lines.combineAll)
          .productR:
            posAndLines.reverse.foldMap: (opaquePos, line) =>
              Resource.fromAutoCloseable(IO(FileInputStream(file.toFile))).use: recompressedIn =>
                IO:
                  val in = recompressor.decompressingInputStream(recompressedIn)
                  recompressedIn.getChannel.position(opaquePos.toLong)
                  val readChunk = Chunk.array(in.readNBytes(line.length))
                  logger.info(s"$opaquePos line=»${line.utf8String.stripLineEnd}« readChunk=»${
                    readChunk.utf8String.stripLineEnd}«")
                  assert(readChunk.utf8String == line.utf8String) // For readable error message
                  assert(readChunk == line)


private object RecompressorTest:
  private val logger = Logger[RecompressorTest]
