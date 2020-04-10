package com.sos.jobscheduler.common.scalautil.xmls

import com.sos.jobscheduler.common.scalautil.FileUtils.syntax.RichPath
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createTempFile, delete}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileSourceTest extends AnyFreeSpec {

  "FileSource" in {
    val file = createTempFile("test-", ".tmp")
    file := "<test/>"
    val source = new FileSource(file)
    assert(scala.io.Source.fromInputStream(source.getInputStream)(UTF_8).getLines().mkString == "<test/>")
    source.close()
    delete(file)  // Under Windows, file must be closed now
  }
}
