package com.sos.scheduler.engine.taskserver.moduleapi

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ModuleLanguageTest extends FreeSpec {

  "ModuleLanguage" in {
    intercept[IllegalArgumentException] { ModuleLanguage("shelL") }
    ModuleLanguage("")
    ModuleLanguage("shell")
    ModuleLanguage("java:javascript")
  }
}
