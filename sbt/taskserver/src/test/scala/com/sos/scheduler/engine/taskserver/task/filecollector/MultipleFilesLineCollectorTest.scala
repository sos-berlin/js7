package com.sos.scheduler.engine.taskserver.task.filecollector

import com.sos.scheduler.engine.common.scalautil.Closers._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createTempFile, delete}
import org.scalatest.FreeSpec
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
final class MultipleFilesLineCollectorTest extends FreeSpec  {

  "FileLogger" in {
    withCloser { implicit closer ⇒
      val files = List.fill(2) { createTempFile("test-", ".tmp") withCloser delete }
      val writers = files map { f ⇒ new OutputStreamWriter(new FileOutputStream(f), UTF_8).closeWithCloser }
      val fileLogger = new MultipleFilesLineCollector(List("one", "two") zip files, UTF_8, batchThreshold = 100).closeWithCloser

      for (_ ← 1 to 1000) {
        val testLines = List.fill(10) { randomString(100) }
        for (line ← testLines) writers(Random.nextInt(writers.size)).write(s"$line\n")
        for (w ← writers) w.flush()
        val (files, batches) = fileLogger.nextBatchIterator.toList.unzip
        assert(files.toSet == files.toSet)
        assert(batches.flatten.toSet == testLines.toSet)
      }
    }
  }

  private def randomString(size: Int): String = {
    def randomCodePoint() = Random.nextInt(10) match {
      case 0 ⇒ 0x20 + Random.nextInt(95)
      case _ ⇒ 0x120 + Random.nextInt(0xf00)
    }
    "" ++ Iterator.fill(size) { randomCodePoint().toChar }
  }
}
