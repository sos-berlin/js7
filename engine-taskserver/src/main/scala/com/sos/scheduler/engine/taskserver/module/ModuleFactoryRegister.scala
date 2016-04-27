package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.taskserver.module.ModuleFactoryRegister._
import com.sos.scheduler.engine.taskserver.module.javamodule.{JavaScriptModule, StandardJavaModule}
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import scala.collection.immutable

/**
  * Register for the different [[ModuleFactory]] (JobScheduler configuration &lt;script>).
  *
  * @author Joacim Zschimmer
  */
final class ModuleFactoryRegister(val moduleFactories: immutable.Seq[ModuleFactory]) {

  def newModule(raw: RawModuleArguments): Module =
    toModuleArguments(raw).newModule()

  def toModuleArguments(raw: RawModuleArguments): ModuleArguments =
    moduleFactory(raw).toModuleArguments(raw)

  def moduleFactory(arguments: RawModuleArguments): ModuleFactory =
    moduleFactories collectFirst { case o if o.toModuleArguments isDefinedAt arguments ⇒ o } getOrElse {
      throw new UnsupportedRawModuleArgumentsException(arguments.language)
    }
}

object ModuleFactoryRegister {
  lazy val StandardModuleTypes = List(ShellModule, StandardJavaModule, JavaScriptModule)

  private[module] final class UnsupportedRawModuleArgumentsException(language: ModuleLanguage)
  extends NoSuchElementException(s"Unsupported script language '$language' or unsupported argument combination")
}
