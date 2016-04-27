package com.sos.scheduler.engine.taskserver.module.dotnet

import com.sos.scheduler.engine.taskserver.dotnet.api.{DotnetModuleInstanceFactory, DotnetModuleReference, TaskContext}
import com.sos.scheduler.engine.taskserver.module.javamodule.{ApiModule, JavaModule}
import com.sos.scheduler.engine.taskserver.module.{ModuleArguments, ModuleFactory, ModuleLanguage, NamedInvocables, RawModuleArguments, Script}

/**
  * @author Joacim Zschimmer
  */
final class DotnetModule private[dotnet](val arguments: DotnetModule.Arguments, factory: DotnetModuleInstanceFactory)
extends ApiModule {
  import DotnetModule._

  def newJobInstance(namedInvocables: NamedInvocables) =
    factory.newInstance(classOf[sos.spooler.Job_impl], namedInvocablesToTaskContext(namedInvocables), arguments.reference)

  def newMonitorInstance(namedInvocables: NamedInvocables) =
    factory.newInstance(classOf[sos.spooler.Monitor_impl], namedInvocablesToTaskContext(namedInvocables), arguments.reference)
}

object DotnetModule {
  trait DotnetModuleLanguage extends ModuleLanguage

  case object DotnetClassModuleLanguage extends DotnetModuleLanguage {
    val string = "dotnet"
  }

  case object PowershellModuleLanguage extends DotnetModuleLanguage {
    val string = "PowerShell"
  }

  final class Factory(factory: DotnetModuleInstanceFactory) extends ModuleFactory {
    def toModuleArguments: PartialFunction[RawModuleArguments, ModuleArguments] = {
      case RawModuleArguments(PowershellModuleLanguage, None, script, None, None) ⇒
        Arguments(this, DotnetModuleReference.Powershell(script = script.string))

      case RawModuleArguments(DotnetClassModuleLanguage, None, Script.Empty, Some(dll), Some(className)) ⇒
        Arguments(this, DotnetModuleReference.DotnetClass(dll = dll, className = className))
    }

    def newModule(arguments: ModuleArguments) =
      new DotnetModule(arguments.asInstanceOf[Arguments], factory)
  }

  private def namedInvocablesToTaskContext(namedInvocables: NamedInvocables) = TaskContext(
    JavaModule.spooler_log(namedInvocables),
    JavaModule.spooler_task(namedInvocables),
    JavaModule.spooler_job(namedInvocables),
    JavaModule.spooler(namedInvocables))

  final case class Arguments(val moduleFactory: ModuleFactory, reference: DotnetModuleReference) extends ModuleArguments
}
