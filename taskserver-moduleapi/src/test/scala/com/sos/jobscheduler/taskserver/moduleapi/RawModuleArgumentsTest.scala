package com.sos.scheduler.engine.taskserver.moduleapi

import java.nio.file.{Path, Paths}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RawModuleArgumentsTest extends FreeSpec {
  "toString" in {
    val raw = RawModuleArguments(ModuleLanguage("language"), Some("JAVA_CLASS"), Script("SCRIPT"), Some(Paths.get("DLL")), Some("DOTNET-CLASS"))
    assert(raw.toString == "RawModuleArguments(language=language java_class=JAVA_CLASS dll=DLL dotnet_class=DOTNET-CLASS script=SCRIPT)")
  }
}
