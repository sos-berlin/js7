package js7.journal.write

import java.io.IOException
import java.nio.file.Files.{createTempDirectory, createTempFile, delete}
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.test.OurTestSuite
import org.scalatest.BeforeAndAfterAll

/**
  * @author Joacim Zschimmer
  */
final class FileJsonWriterTest extends OurTestSuite, BeforeAndAfterAll:

  private lazy val file = createTempFile("FileJsonWriterTest-", ".tmp")
  private lazy val writer = new FileJsonWriter(file)

  override def afterAll() =
    writer.close()
    delete(file)
    super.afterAll()

  "write" in:
    writer.write(ByteArray("FIRST."))
    assert(!writer.isFlushed && !writer.isSynced)

  "flush" in:
    writer.flush()
    assert(file.contentString == "FIRST.\n")
    assert(writer.isFlushed && !writer.isSynced)

  "Exception with complete filename is not wrapped" in:
    val dir = createTempDirectory("FileJsonWriterTest")
    intercept[IOException]:
      new FileJsonWriter(dir / "not-existant" / "file")
    delete(dir)
