package js7.base.convert

import js7.base.convert.AsJava._
import js7.base.convert.ConvertiblePartialFunctions.ImplicitConvertablePF
import java.nio.file.{Path, Paths}
import org.scalatest

/**
  * @author Joacim Zschimmer
  */
final class AsJavaTest extends scalatest.freespec.AnyFreeSpec {

  "as[Path]" in {
    assert(Map("path" -> "/PATH").as[Path]("path") == Paths.get("/PATH"))
  }
}
