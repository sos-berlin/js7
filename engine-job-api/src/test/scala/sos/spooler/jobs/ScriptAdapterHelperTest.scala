package sos.spooler.jobs

import ScriptAdapterHelper._
import org.scalatest.FunSuite
import org.scalatest.Matchers._

final class ScriptAdapterHelperTest extends FunSuite {

  test("ScriptAdapterJob.parseLanguageParameter") {
    intercept[RuntimeException] { parseLanguageParameter("INVALID-PREFIX:language") }
    intercept[RuntimeException] { parseLanguageParameter("java") }
    intercept[RuntimeException] { parseLanguageParameter(":language") }
    parseLanguageParameter("java:language") should equal (new Parameters("language", true))
    parseLanguageParameter("java:") should equal (new Parameters("", true))
    parseLanguageParameter("javax.script:language") should equal (new Parameters("language", false))
  }
}
