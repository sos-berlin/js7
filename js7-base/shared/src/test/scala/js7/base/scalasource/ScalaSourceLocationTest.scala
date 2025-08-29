package js7.base.scalasource

import js7.base.scalasource.ScalaSourceLocation.sourceCodeToString
import org.scalatest.freespec.AnyFreeSpec

final class ScalaSourceLocationTest extends AnyFreeSpec:

  "ScalaSourceLocation" in:
    def f()(using loc: ScalaSourceLocation) = loc.toString
    assert(f() == "ScalaSourceLocationTest.scala:10")

  "sourceCodeToString" in :
    val string = sourceCodeToString:
      1 +
        2
    assert(string == "»1 +⏎2«")
