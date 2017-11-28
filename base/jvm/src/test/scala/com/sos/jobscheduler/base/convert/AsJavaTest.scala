package com.sos.jobscheduler.base.convert

import com.sos.jobscheduler.base.convert.AsJava._
import com.sos.jobscheduler.base.convert.ConvertiblePartialFunctions.ImplicitConvertablePF
import java.nio.file.{Path, Paths}

/**
  * @author Joacim Zschimmer
  */
final class AsJavaTest extends org.scalatest.FreeSpec {

  "as[Path]" in {
    assert(Map("path" → "/PATH").as[Path]("path") == Paths.get("/PATH"))
  }
}
