package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.taskserver.modules.javamodule.{JavaScriptEngineModule, StandardJavaModule}
import com.sos.scheduler.engine.taskserver.modules.shell.ShellModule

/**
  * @author Joacim Zschimmer
  */
package object modules {
  lazy val StandardModuleFactories = List(ShellModule, StandardJavaModule, JavaScriptEngineModule)
}
