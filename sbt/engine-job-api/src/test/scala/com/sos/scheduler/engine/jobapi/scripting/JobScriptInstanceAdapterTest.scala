package com.sos.scheduler.engine.jobapi.scripting

import com.google.common.base.Supplier
import com.google.common.collect.ImmutableMap
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
final class JobScriptInstanceAdapterTest extends FunSuite {

  // Für weitere Tests siehe ScriptInstanceTest mit TestExecutor

  test("language") {
    testLanguage("ECMAScript")
    intercept[RuntimeException] { testLanguage("UNKNOWN-LANGUAGE") }
  }

  private def testLanguage(language: String): Unit = {
    val script = "//"
    val bindingsLazy = new Supplier[ImmutableMap[String, AnyRef]] { protected def get = ImmutableMap.of()}
    new JobScriptInstanceAdapter(language, bindingsLazy, script)
  }
}
