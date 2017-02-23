package com.sos.jobscheduler.taskserver.modules.javamodule

import com.sos.jobscheduler.taskserver.moduleapi.{ModuleLanguage, RawModuleArguments, Script}
import com.sos.jobscheduler.taskserver.modules.javamodule.StandardJavaModule._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StandardJavaModuleTest extends FreeSpec {

  "Wrong language" in {
    val raw = RawModuleArguments(ModuleLanguage("shell"), Some("CLASS"), Script.Empty, None, None)
    assert(!toModuleArguments.isDefinedAt(raw))
  }

  "language=java java_class=CLASS" in {
    val raw = RawModuleArguments(ModuleLanguage("java"), Some("CLASS"), Script.Empty, None, None)
    assert(toModuleArguments(raw) == Arguments("CLASS"))
  }
}
