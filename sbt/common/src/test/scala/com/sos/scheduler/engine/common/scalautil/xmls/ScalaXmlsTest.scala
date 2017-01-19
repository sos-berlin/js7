package com.sos.scheduler.engine.common.scalautil.xmls

import akka.util.ByteString
import com.google.common.base.Charsets.{ISO_8859_1, UTF_8}
import com.google.common.io.Files.{toString ⇒ fileToString}
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXmls.implicits._
import java.io.File
import java.nio.file.Files.delete
import java.nio.file.Path
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ScalaXmlsTest extends FreeSpec {

  "File.xml" in {
    val file = File.createTempFile("sos", ".tmp")
    try {
      file.xml = <å/>
      file.xml shouldEqual <å/>
      fileToString(file, UTF_8) shouldEqual "<?xml version='1.0' encoding='UTF-8'?>\n<å/>"
    }
    finally file.delete()
  }

  "Path.xml" in {
    val path: Path = File.createTempFile("sos", ".tmp").toPath
    try {
      path.xml = <å/>
      path.xml shouldEqual <å/>
      fileToString(path.toFile, UTF_8) shouldEqual "<?xml version='1.0' encoding='UTF-8'?>\n<å/>"
    }
    finally delete(path)
  }

  ".toByteString" in {
    <å/>.toByteString shouldEqual ByteString("<?xml version='1.0' encoding='UTF-8'?><å/>")
  }

  ".toByteString xmlDecl=false" in {
    <å/>.toByteString(xmlDecl = false, encoding = ISO_8859_1) shouldEqual ByteString("""<å/>""", ISO_8859_1.name)
  }
}
