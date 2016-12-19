package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createTempFile, delete}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.io

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FileSourceTest extends FreeSpec {

  "FileSource" in {
    val file = createTempFile("test-", ".tmp")
    file.contentString = "<test/>"
    val source = new FileSource(file)
    assert(io.Source.fromInputStream(source.getInputStream)(UTF_8).getLines().mkString == "<test/>")
    source.close()
    delete(file)  // Under Windows, file must be closed now
  }
}
