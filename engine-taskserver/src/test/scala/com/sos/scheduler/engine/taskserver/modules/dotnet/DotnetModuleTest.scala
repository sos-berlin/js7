package com.sos.scheduler.engine.taskserver.modules.dotnet

import com.sos.scheduler.engine.taskserver.dotnet.api.{DotnetModuleInstanceFactory, DotnetModuleReference}
import com.sos.scheduler.engine.taskserver.moduleapi.{ModuleLanguage, RawModuleArguments, Script}
import com.sos.scheduler.engine.taskserver.modules.dotnet.DotnetModule._
import java.nio.file.Paths
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class DotnetModuleTest extends FreeSpec {

  private val dotnetFactory = new Factory(DotnetModuleInstanceFactory.Unsupported, classDllDirectory = None)

  "Wrong language" in {
    val raw = RawModuleArguments(ModuleLanguage("java:javascript"), None, Script("TEST-SCRIPT"), None, None)
    assert(!dotnetFactory.toModuleArguments.isDefinedAt(raw))
  }

  "powershell" in {
    val raw = RawModuleArguments(ModuleLanguage("powershell"), None, Script("TEST-SCRIPT"), None, None)
    assert(dotnetFactory.toModuleArguments(raw) == Arguments(dotnetFactory, DotnetModuleReference.Powershell("TEST-SCRIPT")))
  }

  "dotnet_class" in {
    val dll = Paths.get("DLL")
    val raw = RawModuleArguments(ModuleLanguage("dotnet"), None, Script("IGNORED"), Some(dll), Some("DOTNET-CLASS"))
    assert(dotnetFactory.toModuleArguments(raw) == Arguments(dotnetFactory, DotnetModuleReference.DotnetClass(dll, "DOTNET-CLASS")))
  }
}
