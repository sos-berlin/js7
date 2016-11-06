package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.taskserver.moduleapi.{ModuleArguments, ModuleFactory, ModuleLanguage, RawModuleArguments}

/**
 * @author Joacim Zschimmer
 */
final class StandardJavaModule(arguments: StandardJavaModule.Arguments) extends JavaClassModule {

  private lazy val clazz = Class.forName(arguments.className)

  protected def newInstance() = clazz.newInstance()
}

object StandardJavaModule extends ModuleFactory {
  val Language = ModuleLanguage("java")

  def toModuleArguments = {
    case args @ RawModuleArguments(ModuleLanguage("java"), Some(javaClassName), script, None, None) ⇒
      new Arguments(className = javaClassName)
    case args @ RawModuleArguments(ModuleLanguage("java"), None, script, None, None) ⇒
      throw new IllegalArgumentException(s"language='$language' requires a class name")
  }

  def newModule(arguments: ModuleArguments) = new StandardJavaModule(arguments.asInstanceOf[Arguments])

  final case class Arguments(className: String) extends JavaModule.Arguments {
    val moduleFactory = StandardJavaModule
  }
}
