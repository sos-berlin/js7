package com.sos.scheduler.engine.taskserver.modules.dotnet

import com.sos.scheduler.engine.taskserver.dotnet.api.{DotnetModuleInstanceFactory, DotnetModuleReference, TaskContext}
import com.sos.scheduler.engine.taskserver.moduleapi.{ModuleArguments, ModuleFactory, ModuleLanguage, NamedIDispatches, RawModuleArguments}
import com.sos.scheduler.engine.taskserver.modules.javamodule.{ApiModule, JavaModule}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final class DotnetModule private[dotnet](val arguments: DotnetModule.Arguments, factory: DotnetModuleInstanceFactory)
extends ApiModule {
  import DotnetModule._

  def newJobInstance(namedIDispatches: NamedIDispatches) =
    factory.newInstance(classOf[sos.spooler.Job_impl], namedIDispatchesToTaskContext(namedIDispatches), arguments.reference)

  def newMonitorInstance(namedIDispatches: NamedIDispatches) =
    factory.newInstance(classOf[sos.spooler.Monitor_impl], namedIDispatchesToTaskContext(namedIDispatches), arguments.reference)
}

object DotnetModule {
  private val DotnetClassLanguage = ModuleLanguage("dotnet")
  private val PowershellLanguage = ModuleLanguage("powershell")

  final class Factory(factory: DotnetModuleInstanceFactory, classDllDirectory: Option[Path]) extends ModuleFactory {
    def toModuleArguments: PartialFunction[RawModuleArguments, ModuleArguments] = {
      case RawModuleArguments(PowershellLanguage, None, script, None, None) ⇒
        Arguments(this, DotnetModuleReference.Powershell(script = script.string))

      case RawModuleArguments(DotnetClassLanguage, None, _, Some(dll), Some(className)) ⇒
        val withDirectory = classDllDirectory map { _ resolve dll } getOrElse dll
        Arguments(this, DotnetModuleReference.DotnetClass(dll = withDirectory, className = className))
    }

    def newModule(arguments: ModuleArguments) =
      new DotnetModule(arguments.asInstanceOf[Arguments], factory)

    override def toString = s"DotnetModule.Factory($factory)"
  }

  private def namedIDispatchesToTaskContext(namedIDispatches: NamedIDispatches) = TaskContext(
    JavaModule.spooler_log(namedIDispatches),
    JavaModule.spooler_task(namedIDispatches),
    JavaModule.spooler_job(namedIDispatches),
    JavaModule.spooler(namedIDispatches))

  final case class Arguments(moduleFactory: ModuleFactory, reference: DotnetModuleReference) extends ModuleArguments
}
