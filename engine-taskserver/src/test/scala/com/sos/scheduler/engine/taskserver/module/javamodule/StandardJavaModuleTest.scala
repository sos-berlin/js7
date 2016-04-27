package com.sos.scheduler.engine.taskserver.module.javamodule

import com.sos.scheduler.engine.taskserver.module.javamodule.StandardJavaModule._
import com.sos.scheduler.engine.taskserver.module.{ModuleLanguage, RawModuleArguments, Script}
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
