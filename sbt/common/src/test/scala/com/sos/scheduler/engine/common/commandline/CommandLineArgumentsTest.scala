package com.sos.scheduler.engine.common.commandline

import com.sos.scheduler.engine.common.commandline.CommandLineArguments.parse
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
 * @author Joacim Zschimmer
 */
final class CommandLineArgumentsTest extends FreeSpec {

  "Flag" - {
    "boolean" in {
      assert(parse(Array("-option")) { _.boolean("-option") })
    }

    "Missing boolean" in {
      assertResult(false) {
        parse(Nil) { a ⇒
          a.boolean("-missing")
        }
      }
    }

    "Invalid used boolean option" in {
      intercept[IllegalArgumentException] {
        parse(Array("-option=value")) { a ⇒
          a.boolean("-option")
        }
      }
        .getMessage shouldEqual "Unknown command line arguments: -option=value"
    }

    "Multiple boolean option" in {
      intercept[IllegalArgumentException] {
        parse(Array("-option", "-option")) { a ⇒
          a.boolean("-option")
        }
      }
        .getMessage shouldEqual "Multiple command line options '-option'"
    }
  }

  "Single value" - {
    "as String" in {
      assertResult("333") {
        parse(List("-int=333")) { a ⇒
          a.as[String]("-int=")
        }
      }
    }

    "as Int" in {
      assertResult(333) {
        parse(List("-int=333")) { a ⇒
          a.as[Int]("-int=")
        }
      }
    }

    "as invalid Int" in {
      intercept[IllegalArgumentException] {
        parse(List("-int=X")) { a ⇒
          a.as[Int]("-int=")
        }
      }
        .getMessage shouldEqual """Invalid command line option '-int=': java.lang.NumberFormatException: For input string: "X""""
    }

    "optionAs Int" in {
      assertResult(Some(333)) {
        parse(List("-int=333")) { a ⇒
          a.optionAs[Int]("-int=")
        }
      }
    }
  }

  "Multiple values" - {
    "multiple Int" in {
      assertResult(List(111, 222)) {
        parse(List("-int=111", "-int=222")) { a ⇒
          a.seqAs[Int]("-int=")
        }
      }
    }

    "Zero multiple Int" in {
      assertResult(Nil) {
        parse(List[String]()) { a ⇒
          a.seqAs[Int]("-int=")
        }
      }
    }
  }

  "keylessValue" - {
    "One" in {
      assertResult("333") {
        parse(List("333")) { a ⇒
          a.keylessValue(0)
        }
      }
    }

    "Two" in {
      assertResult(("222", "111")) {
        parse(List("111", "222")) { a ⇒
          (a.keylessValue(1), a.keylessValue(0))
        }
      }
    }

    "Too many" in {
      intercept[IllegalArgumentException] {
        parse(List("111", "222")) { a ⇒
          a.keylessValue(0)
        }
      }
        .getMessage shouldEqual "Unknown command line arguments: #2"
    }

    "Too few" in {
      intercept[NoSuchElementException] {
        parse(Nil) { a ⇒
          a.keylessValue(0)
        }
      }
        .getMessage shouldEqual "Too few keyless arguments: argument #1 expected"
    }
  }

  "keylessValues" - {
    "Zero" in {
      assertResult(Nil) {
        parse(Nil) { a ⇒
          a.keylessValues
        }
      }
    }

    "One" in {
      assertResult(List("111")) {
        parse(List("111")) { a ⇒
          a.keylessValues
        }
      }
    }

    "Two" in {
      assertResult(List("111", "222")) {
        parse(List("111", "222")) { a ⇒
          a.keylessValues
        }
      }
    }
  }

  "Unused options" in {
    intercept[IllegalArgumentException] {
      parse(List("-option", "-unknown=333")) { a ⇒ }
    }
      .getMessage shouldEqual "Unknown command line arguments: -option -unknown=333"
  }
}
