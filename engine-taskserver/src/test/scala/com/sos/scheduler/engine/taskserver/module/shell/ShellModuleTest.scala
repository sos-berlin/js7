package com.sos.scheduler.engine.taskserver.module.shell

import com.sos.scheduler.engine.taskserver.module.shell.ShellModule._
import com.sos.scheduler.engine.taskserver.module.{ModuleLanguage, RawModuleArguments, Script}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ShellModuleTest extends FreeSpec {

  "Wrong language" in {
    val raw = RawModuleArguments(ModuleLanguage("java:javascript"), None, Script("TEST-SCRIPT"), None, None)
    assert(!toModuleArguments.isDefinedAt(raw))
  }

  "okay" in {
    val raw = RawModuleArguments(ModuleLanguage("shell"), None, Script("TEST-SCRIPT"), None, None)
    assert(toModuleArguments(raw) == Arguments(Script("TEST-SCRIPT")))
  }
}
