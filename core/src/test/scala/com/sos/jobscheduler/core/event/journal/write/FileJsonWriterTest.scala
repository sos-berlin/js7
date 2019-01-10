package com.sos.jobscheduler.core.event.journal.write

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import java.io.IOException
import java.nio.file.Files.{createTempDirectory, createTempFile, delete}
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * @author Joacim Zschimmer
  */
final class FileJsonWriterTest extends FreeSpec with BeforeAndAfterAll
{
  private lazy val file = createTempFile("FileJsonWriterTest-", ".tmp")
  private lazy val writer = new FileJsonWriter(file)

  override def afterAll() = {
    writer.close()
    delete(file)
    super.afterAll()
  }

  "write" in {
    writer.write(ByteString("FIRST."))
    assert(!writer.isFlushed && !writer.isSynced)
  }

  "flush" in {
    writer.flush()
    assert(file.contentString == "FIRST.\n")
    assert(writer.isFlushed && !writer.isSynced)
  }

  "Exception with complete filename is not wrapped" in {
    val dir = createTempDirectory("FileJsonWriterTest")
    intercept[IOException] {
      new FileJsonWriter(dir / "not-existant" / "file")
    }
    delete(dir)
  }
}
