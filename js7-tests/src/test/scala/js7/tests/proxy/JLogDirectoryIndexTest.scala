package js7.tests.proxy

import cats.effect.IO
import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.format.DateTimeFormatter
import java.time.{ZoneId, ZonedDateTime}
import java.util.zip.GZIPOutputStream
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.time.JavaTime.extensions.+
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.proxy.javaapi.JProxyContext
import js7.tests.proxy.JLogDirectoryIndexTest.*

final class JLogDirectoryIndexTest extends OurAsyncTestSuite:

  "Test" in :
    temporaryDirectoryResource[IO]("LogDirectoryIndexTest-").use: dir =>
      val startInstant = ZonedDateTime.parse("2026-03-01T00:00:00.000+02").toInstant
      IO:
        var i = 0
        (0 until 3).foreach: d =>
          val midnight = startInstant + 24.h * d
          (0 until 3).foreach: h =>
            val hour = midnight + h.h
            val gzFile = dir / s"js7-${hour.atZone(zoneId).toLocalDate}-$h.log.gz"
            autoClosing(
              new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(gzFile.toFile)))
            ): out =>
              out.write:
                (headerTimestampFormatter.format(hour.atZone(zoneId)) + " HEADER\n").getBytes(UTF_8)
              (1 to 3).foreach: s =>
                i += 1
                out.write:
                  s"${timestampFormatter.format((hour + s.s).atZone(zoneId))} info LogDirectoryIndexTest - MESSAGE $i\n"
                    .getBytes(UTF_8)
      .productR:
        JProxyContext.resource().use: jProxy =>
          IO.fromCompletableFuture:
            IO:
              JLogDirectoryIndexTester.test(jProxy, zoneId, dir)
          .as(succeed)


object JLogDirectoryIndexTest:
  private val logger = Logger[this.type]
  private val zoneId = ZoneId.of("Europe/Mariehamn")
  private val headerTimestampFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSXX")
  private val timestampFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
