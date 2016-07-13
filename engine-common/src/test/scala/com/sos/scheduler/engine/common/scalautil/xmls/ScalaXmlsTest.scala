package com.sos.scheduler.engine.common.scalautil.xmls

import akka.util.ByteString
import com.google.common.base.Charsets.{ISO_8859_1, UTF_8}
import com.google.common.io.Files
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXmls.implicits._
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ScalaXmlsTest extends FreeSpec {

  ".xml" in {
    val f = File.createTempFile("sos", ".tmp")
    try {
      f.xml = <å/>
      f.xml shouldEqual <å/>
      Files.toString(f, UTF_8) shouldEqual "<?xml version='1.0' encoding='UTF-8'?>\n<å/>"
    }
    finally f.delete()
  }

  ".toByteString" in {
    <å/>.toByteString shouldEqual ByteString("<?xml version='1.0' encoding='UTF-8'?><å/>")
  }

  ".toByteString xmlDecl=false" in {
    <å/>.toByteString(xmlDecl = false, encoding = ISO_8859_1) shouldEqual ByteString("""<å/>""", ISO_8859_1.name)
  }
}
