package com.sos.jobscheduler.taskserver.modules.javamodule

import com.sos.jobscheduler.taskserver.moduleapi.{ModuleLanguage, RawModuleArguments, Script}
import com.sos.jobscheduler.taskserver.modules.javamodule.JavaScriptEngineModule.{Arguments, _}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaScriptEngineModuleTest extends FreeSpec {

  "Wrong language" in {
    val raw = RawModuleArguments(ModuleLanguage("java"), None, Script.Empty, None, None)
    assert(!toModuleArguments.isDefinedAt(raw))
  }

  "okay" in {
    val raw = RawModuleArguments(ModuleLanguage("java:test"), None, Script("TEST-SCRIPT"), None, None)
    assert(toModuleArguments(raw) == Arguments("java:test", Script("TEST-SCRIPT")))
  }
}
