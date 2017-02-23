package com.sos.scheduler.engine.taskserver.task.filecollector

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import java.io.{BufferedReader, FileInputStream, FileOutputStream, InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createTempFile, delete}
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class LineBatchCollectorTest extends FreeSpec {

  "LineBatchCollector" in {
    withCloser { implicit closer ⇒
      val file = createTempFile("test-", ".tmp") withCloser delete
      autoClosing(new OutputStreamWriter(new FileOutputStream(file), UTF_8)) { writer ⇒
        autoClosing(new BufferedReader(new InputStreamReader(new FileInputStream(file), UTF_8))) { reader ⇒
          val fileLogger = new LineBatchCollector(reader, threshold = 5)
          for ((writtenLines, readBatches) ← List(
            List("1a", "1b", "1c") → List(List("1a", "1b"), List("1c")),
            List("2a", "2b") → List(List("2a", "2b"))))
          {
            for (line ← writtenLines) writer.write(s"$line\n")
            writer.flush()
              fileLogger.nextBatchIterator().toList shouldEqual readBatches
          }
        }
      }
    }
  }
}
