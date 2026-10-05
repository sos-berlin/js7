package js7.base.scalasource

import js7.base.scalasource.ScalaSourceLocation.sourceCodeToString
import js7.base.test.OurTestSuite

final class ScalaSourceLocationTest extends OurTestSuite:

  private val logger = Logger[this.type]

  "ScalaSourceLocation" in:
    def f()(using loc: ScalaSourceLocation) = loc.toString
    logger.info(f())
    //if isIntelliJIdea then
      assert(f() == ".(ScalaSourceLocationTest.scala:14)")
    //else
    //  assert(f() == "ScalaSourceLocationTest.scala:16")

  "sourceCodeToString" in :
    val string = sourceCodeToString:
      1 +
        2
    assert(string == "»1 +⏎2«")
