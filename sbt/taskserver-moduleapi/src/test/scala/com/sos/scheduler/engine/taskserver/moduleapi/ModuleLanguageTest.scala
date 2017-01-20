package com.sos.scheduler.engine.taskserver.moduleapi

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ModuleLanguageTest extends FreeSpec {

  "ModuleLanguage" in {
    intercept[IllegalArgumentException] { ModuleLanguage("shelL") }
    ModuleLanguage("")
    ModuleLanguage("shell")
    ModuleLanguage("java:javascript")
  }
}
