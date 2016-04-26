package com.sos.scheduler.engine.taskserver.module.javamodule

import com.sos.scheduler.engine.taskserver.module.ModuleArguments.JavaModuleArguments

/**
 * @author Joacim Zschimmer
 */
final class StandardJavaModule(val arguments: JavaModuleArguments) extends JavaClassModule {

  private lazy val clazz = Class.forName(arguments.className)

  protected def newInstance() = clazz.newInstance()
}
