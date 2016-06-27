package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.moduleapi.{ModuleLanguage, RawModuleArguments, Script}
import com.sos.scheduler.engine.taskserver.modules.javamodule.StandardJavaModule._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
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
