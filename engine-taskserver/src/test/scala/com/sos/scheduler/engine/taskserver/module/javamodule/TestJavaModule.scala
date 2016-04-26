package com.sos.scheduler.engine.taskserver.module.javamodule

import com.sos.scheduler.engine.taskserver.module.{ModuleArguments, ModuleType}

/**
  * @author Joacim Zschimmer
  */
final class TestJavaModule(arguments: TestJavaModule.Arguments) extends JavaClassModule {
  def newInstance() = arguments.newInstance()
}

object TestJavaModule extends ModuleType {
  def toModuleArguments = throw new NotImplementedError  // Here, we do not use a ModuleRegister

  def newModule(arguments: ModuleArguments) = new TestJavaModule(arguments.asInstanceOf[Arguments])

  def arguments(newInstance: ⇒ Any) = Arguments(() ⇒ newInstance)

  final case class Arguments(newInstance: () ⇒ Any) extends JavaModule.Arguments {
    val moduleType = TestJavaModule
  }
}
