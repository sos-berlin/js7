package js7.common.scalautil.xmls

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createTempFile, delete}
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class FileSourceTest extends OurTestSuite:

  "FileSource" in:
    val file = createTempFile("test-", ".tmp")
    file := "<test/>"
    val source = new FileSource(file)
    assert(scala.io.Source.fromInputStream(source.getInputStream)(UTF_8).getLines().mkString == "<test/>")
    source.close()
    delete(file)  // Under Windows, file must be closed now
