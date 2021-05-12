package js7.common.scalautil.xmls

import akka.util.ByteString
import java.io.File
import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.Files.delete
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax._
import js7.common.scalautil.xmls.ScalaXmls.implicits._
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class ScalaXmlsTest extends AnyFreeSpec
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
