package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.taskserver.module.ModuleRegister._
import com.sos.scheduler.engine.taskserver.module.javamodule.{JavaScriptModule, StandardJavaModule}
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import scala.collection.immutable

/**
  * Register for the different [[ModuleType]] (JobScheduler configuration &lt;script>).
  *
  * @author Joacim Zschimmer
  */
final class ModuleRegister(val moduleTypes: immutable.Seq[ModuleType]) {

  def newModule(raw: RawModuleArguments): Module =
    toModuleArguments(raw).newModule()

  def toModuleArguments(raw: RawModuleArguments): ModuleArguments =
    moduleType(raw).toModuleArguments(raw)

  def moduleType(arguments: RawModuleArguments): ModuleType =
    moduleTypes collectFirst { case o if o.toModuleArguments isDefinedAt arguments ⇒ o } getOrElse {
      throw new UnsupportedRawModuleArgumentsException(arguments.language)
    }
}

object ModuleRegister {
  lazy val StandardModuleTypes = List(ShellModule, StandardJavaModule, JavaScriptModule)

  private[module] final class UnsupportedRawModuleArgumentsException(language: ModuleLanguage)
  extends NoSuchElementException(s"Unsupported script language '$language' or unsupported argument combination")
}
