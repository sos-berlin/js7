package com.sos.scheduler.engine.taskserver.moduleapi

import com.sos.scheduler.engine.taskserver.moduleapi.ModuleFactoryRegister._
import scala.collection.immutable

/**
  * Register for the different [[ModuleFactory]] (JobScheduler configuration &lt;script>).
  *
  * @author Joacim Zschimmer
  */
final class ModuleFactoryRegister(val moduleFactories: immutable.Seq[ModuleFactory]) {

  def newModule(raw: RawModuleArguments): Module = {
    val factory = moduleFactory(raw)
    factory.newModule(factory.toModuleArguments(raw))
  }

  def toModuleArguments(raw: RawModuleArguments): ModuleArguments =
    moduleFactory(raw).toModuleArguments(raw)

  def moduleFactory(arguments: RawModuleArguments): ModuleFactory =
    moduleFactories collectFirst { case o if o.toModuleArguments isDefinedAt arguments ⇒ o } getOrElse {
      throw new UnsupportedRawModuleArgumentsException(arguments)
    }
}

object ModuleFactoryRegister {
  private[moduleapi] final class UnsupportedRawModuleArgumentsException(raw: RawModuleArguments)
  extends NoSuchElementException(s"Unsupported script language '${raw.language}' or unsupported argument combination $raw")
}
