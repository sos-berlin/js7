package com.sos.scheduler.engine.taskserver.modules.shell

import com.sos.scheduler.engine.taskserver.module.{ModuleLanguage, RawModuleArguments, Script}
import com.sos.scheduler.engine.taskserver.modules.shell.ShellModule._
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
