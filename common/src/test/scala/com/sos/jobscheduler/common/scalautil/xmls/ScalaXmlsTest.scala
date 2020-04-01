package com.sos.jobscheduler.common.scalautil.xmls

import akka.util.ByteString
import com.google.common.base.Charsets.ISO_8859_1
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits._
import java.io.File
import java.nio.file.Files.delete
import java.nio.file.Path
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class ScalaXmlsTest extends FreeSpec
{
  "Path.xml" in {
    val path: Path = File.createTempFile("sos", ".tmp").toPath
    try {
      path.xml = <å/>
      assert(path.xml == <å/>)
      assert(path.contentString == "<?xml version='1.0' encoding='UTF-8'?>\n<å/>")
    }
    finally delete(path)
  }

  ".toByteString" in {
    assert(<å/>.toByteString == ByteString("<?xml version='1.0' encoding='UTF-8'?><å/>"))
  }

  ".toByteString xmlDecl=false" in {
    assert(<å/>.toByteString(xmlDecl = false, encoding = ISO_8859_1) == ByteString("""<å/>""", ISO_8859_1.name))
  }
}
