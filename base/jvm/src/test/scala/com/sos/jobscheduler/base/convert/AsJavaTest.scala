package com.sos.jobscheduler.base.convert

import com.sos.jobscheduler.base.convert.AsJava._
import com.sos.jobscheduler.base.convert.ConvertiblePartialFunctions.ImplicitConvertablePF
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
