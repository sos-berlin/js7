package js7.base.io.file

import cats.effect.IO
import cats.effect.std.Queue
import java.nio.file.Files
import java.nio.file.Files.deleteIfExists
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.LogFileReader.{UniqueHeaderSize, growingLogFileStream}
import js7.base.io.file.LogFileReaderTest.*
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.foldMap
import org.scalatest.compatible.Assertion
import scala.util.Random

final class LogFileReaderTest extends OurAsyncTestSuite:

  "growingLogFileStream" in :
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
                    assert(more.length >= UniqueHeaderSize)
                    file ++= more
                    queue.take.timeout(9.s).map: chunk =>
                      assert(chunk == more)

            growingLogFileStream[ByteArray](file, byteChunkSize = 1024, pollDuration = 10.ms)
              .unchunks
              .takeWhile(_ != Stop)
              .foreach(queue.offer)
              .compile.drain
              .both:
                writeAndRead("A") // First log file //
                  .productL:
                    // Change log file //
                    IO.blocking:
                      logger.info("Changing log file")
                      Files.delete(file)
                  .productL:
                    writeAndRead("B") // Second log file //
                  .productL:
                    IO:
                      file := Stop
              .map(_._2)


object LogFileReaderTest:
  private val logger = Logger[this.type]
  private val Stop = ByteArray("STOP" + "." * UniqueHeaderSize)
