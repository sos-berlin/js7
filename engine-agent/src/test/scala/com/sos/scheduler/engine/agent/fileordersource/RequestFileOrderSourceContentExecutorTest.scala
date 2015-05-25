package com.sos.scheduler.engine.agent.fileordersource

import com.google.common.io.Files.touch
import com.sos.scheduler.engine.agent.data.commands.{FileOrderSourceContent, _}
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.nio.file.Files.{createTempDirectory, setLastModifiedTime}
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Paths}
import java.time.{ZoneId, ZonedDateTime}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.concurrent.Futures
import org.scalatest.junit.JUnitRunner
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class RequestFileOrderSourceContentExecutorTest extends FreeSpec with Futures {

  "RequestFileOrderSourceContent" in {
    withCloser { closer ⇒
      val dir = createTempDirectory("agent-")
      closer.onClose { Files.delete(dir) }
      val aTime = ZonedDateTime.of(2015, 5, 1, 12, 0, 0, 0, ZoneId.of("UTC")).toInstant
      val xTime = aTime + 2.s
      val cTime = aTime + 4.s
      val expectedResponse = FileOrderSourceContent(List(
        FileOrderSourceContent.Entry((dir / "a").toString, aTime.toEpochMilli),
        FileOrderSourceContent.Entry((dir / "x").toString, xTime.toEpochMilli),
        FileOrderSourceContent.Entry((dir / "c").toString, cTime.toEpochMilli)))
      for (entry ← expectedResponse.files) {
        val path = Paths.get(entry.path)
        touch(path)
        setLastModifiedTime(path, FileTime.fromMillis(entry.lastModifiedTime))
        closer.onClose {Files.delete(path)}
      }
      val knownFile = dir / "known"
      touch(knownFile)
      closer.onClose { Files.delete(knownFile) }

      val future = RequestFileOrderSourceContentExecutor.apply(RequestFileOrderSourceContent(
        directory = dir.toString,
        regex = ".*",
        durationMillis = Long.MaxValue,
        knownFiles = Set(knownFile.toString)))
      val response = Await.result(future, 10.seconds)
      assert(response == expectedResponse)
    }
  }
}
