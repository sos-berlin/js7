package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.moduleapi.{ModuleArguments, ModuleFactory}

/**
  * @author Joacim Zschimmer
  */
final class TestJavaModule(arguments: TestJavaModule.Arguments) extends JavaClassModule {
  def newInstance() = arguments.newInstance()
}

object TestJavaModule extends ModuleFactory {
  def toModuleArguments = throw new NotImplementedError  // Here, we do not use a ModuleFactoryRegister

  def newModule(arguments: ModuleArguments) = new TestJavaModule(arguments.asInstanceOf[Arguments])

  def arguments(newInstance: ⇒ Any) = Arguments(() ⇒ newInstance)

  final case class Arguments(newInstance: () ⇒ Any) extends JavaModule.Arguments {
    val moduleFactory = TestJavaModule
  }
}
