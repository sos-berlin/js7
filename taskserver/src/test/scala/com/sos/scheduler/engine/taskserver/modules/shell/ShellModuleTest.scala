package com.sos.scheduler.engine.taskserver.modules.shell

import com.sos.scheduler.engine.taskserver.moduleapi.{ModuleLanguage, RawModuleArguments, Script}
import com.sos.scheduler.engine.taskserver.modules.shell.ShellModule._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ShellModuleTest extends FreeSpec {

  private val unusedRichProcessStartSynchronizer: RichProcessStartSynchronizer = null
  private val shellModuleFactory = new ShellModule.Factory(unusedRichProcessStartSynchronizer)

  "Wrong language" in {
    val raw = RawModuleArguments(ModuleLanguage("java:javascript"), None, Script("TEST-SCRIPT"), None, None)
    assert(!shellModuleFactory.toModuleArguments.isDefinedAt(raw))
  }

  "okay" in {
    val raw = RawModuleArguments(ModuleLanguage("shell"), None, Script("TEST-SCRIPT"), None, None)
    assert(shellModuleFactory.toModuleArguments(raw) == Arguments(shellModuleFactory, Script("TEST-SCRIPT")))
  }
}
