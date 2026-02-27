package js7.proxy.log

import cats.effect.IO
import java.time.ZoneId
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.temporaryFileResource
import js7.base.test.OurAsyncTestSuite
import scala.jdk.CollectionConverters.*

final class JLogFileIndexTest extends OurAsyncTestSuite:

  "Test" in:
    val zoneId = ZoneId.of("Europe/Mariehamn")
    temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
      IO.defer:
        val lines = Vector(
          s"2026-02-12 14:00:00.000+0200 HEADER ...\n",
          s"2026-02-12 14:00:01.000 info [thread] class - MESSAGE 1\n",
          s"2026-02-12 14:00:02.000 info [thread] class - MESSAGE 2\n",
          s"2026-02-12 14:00:03.000 info [thread] class - MESSAGE 3\n",
          s"2026-02-12 14:00:04.000 info [thread] class - MESSAGE 4\n",
          s"2026-02-12 14:00:05.000 info [thread] class - MESSAGE 5\n",
          s"2026-02-12 14:00:06.000 info [thread] class - MESSAGE 6\n")
        file := lines.mkString

        IO.fromCompletableFuture:
          IO:
            JLogFileIndexTester.test(file, zoneId, lines.asJava)
        .as(succeed)
