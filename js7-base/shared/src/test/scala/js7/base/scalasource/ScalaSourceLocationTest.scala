package js7.base.scalasource

import org.scalatest.freespec.AnyFreeSpec

final class ScalaSourceLocationTest extends AnyFreeSpec:

  "test" in:
    def f()(using loc: ScalaSourceLocation) = loc.toString
    assert(f() == "ScalaSourceLocationTest.scala:9")
