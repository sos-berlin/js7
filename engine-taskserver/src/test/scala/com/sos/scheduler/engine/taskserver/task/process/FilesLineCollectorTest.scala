package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.Closers._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Files.createTempFile
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class FilesLineCollectorTest extends FreeSpec  {

  "FileLogger" in {
    withCloser { implicit closer ⇒
      val files = List.fill(2) { createTempFile("test-", ".tmp") }
      for (f ← files) closer.onClose { Files.delete(f) }
      val writers = files map { f ⇒ new OutputStreamWriter(new FileOutputStream(f), UTF_8).closeWithCloser }
      val fileLogger = new FilesLineCollector(files, UTF_8).closeWithCloser

      for (_ ← 1 to 3) {
        val testLines = List.fill(10) { Random.nextString(10) }
        for (line ← testLines) writers(Random.nextInt(writers.size)).write(s"$line\n")
        for (w ← writers) w.flush()
        val (files, lines) = fileLogger.nextLinesIterator.toList.unzip
        files.toSet shouldEqual files.toSet
        lines.toSet shouldEqual testLines.toSet
      }
    }
  }
}
