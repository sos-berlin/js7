package js7.base.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

final class IOUtilsTest extends AnyFreeSpec
{
  "copyStream" in {
    val bytes = Random.nextString(10001).getBytes(UTF_8)
    val in = new ByteArrayInputStream(bytes)
    val out = new ByteArrayOutputStream
    IOUtils.copyStream(in, out)
    assert(out.toByteArray sameElements bytes)
  }
}
