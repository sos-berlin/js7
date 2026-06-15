package js7.base.log.reader

import cats.effect.IO
import cats.effect.std.Queue
import java.nio.file.Files.deleteIfExists
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.log.reader.LogFileReader.{UniqueHeaderSize, streamGrowingLogFile}
import js7.base.log.reader.LogFileReaderTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.foldMap
import org.scalatest.compatible.Assertion
import scala.util.Random

final class LogFileReaderTest extends OurAsyncTestSuite:

  "streamGrowingLogFile" in :
    temporaryFileResource[IO]("LogFileReaderTest").use: file =>
      (1 to 2).foldMap: i =>
        IO.defer:
          logger.debug(s"——— $i " * 10)
          deleteIfExists(file)
          file := ""
          Queue.unbounded[IO, ByteArray].flatMap: queue =>
            def writeAndRead(key: String): IO[Assertion] =
              IO.sleep(100.ms) *>
                (1 to 20).foldMap: j =>
                  IO.defer:
                    val more = ByteArray:
                      key + " " +
                        (s"loop=$i index=$j " + "." * UniqueHeaderSize).take(UniqueHeaderSize) +
                        (1 to Random.nextInt(10)).toVector.map(i => Random.nextPrintableChar()).mkString +
                        "\n"
                    logger.info(s"Write $more")
                    assert(more.length >= UniqueHeaderSize)
                    file ++= more
                    queue.take.timeout(9.s).map: chunk =>
                      assert(chunk == more)

            streamGrowingLogFile[ByteArray](file, byteChunkSize = 1024, poll = 10.ms)
              .foreach(queue.offer)
              .compile.drain
              .both:
                writeAndRead("A") // First log file //
                  .productL:
                    // Change log file - this terminates the Stream //
                    IO.blocking:
                      logger.info("Changing log file")
                      file := "+" * UniqueHeaderSize
              .map(_._2)


object LogFileReaderTest:
  private val logger = Logger[this.type]
