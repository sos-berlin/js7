package com.sos.scheduler.engine.common.commandline

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class CommandLineArgumentsTest extends FreeSpec {

  "CommandLineArguments" in {
    val a = CommandLineArguments(Array("-option", "-int=1", "-multi-int=11", "-single=SINGLE", "-multi-int=22", "UNNAMED-1", "UNNAMED-2"))
    assert(a.boolean("-option"))
    assert(a.int("-int=") == 1)
    assert(a.string("-single=") == "SINGLE")
    assert(a.asConvertedList("-multi-int=") { _.toInt } == Vector(11, 22))
    assert(a.namelessValues == Vector("UNNAMED-1", "UNNAMED-2"))
    a.requireNoMoreArguments()
  }

  "namelessValue" in {
    val a = CommandLineArguments(Array("-option", "UNNAMED-1", "UNNAMED-2"))
    assert(a.boolean("-option"))
    assert(a.namelessValue(0) == "UNNAMED-1")
    intercept[IllegalArgumentException] { a.requireNoMoreArguments() } .getMessage should include ("#2")
    assert(a.namelessValue(1) == "UNNAMED-2")
    intercept[NoSuchElementException] { a.namelessValue(2) } .getMessage should include ("#3")
    a.requireNoMoreArguments()
  }

  "-int=x shoud be rejected" in {
    val a = CommandLineArguments(Array("-int=x"))
    intercept[IllegalArgumentException] { a.int("-int=") } .getMessage contains "-int="
  }

  "requireNoMoreArguments" in {
    val a = CommandLineArguments(Array("-int=1"))
    intercept[IllegalArgumentException] { a.requireNoMoreArguments() }
  }
}
