package com.sos.scheduler.engine.taskserver.modules.dotnet

import com.sos.scheduler.engine.taskserver.dotnet.api.{DotnetModuleInstanceFactory, DotnetModuleReference, TaskContext}
import com.sos.scheduler.engine.taskserver.module.{ModuleArguments, ModuleFactory, ModuleLanguage, NamedInvocables, RawModuleArguments}
import com.sos.scheduler.engine.taskserver.modules.javamodule.{ApiModule, JavaModule}
import java.nio.file.Path

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
  final val DotnetClassLanguage = ModuleLanguage("dotnet")
  final val PowershellLanguage = ModuleLanguage("powershell")

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

  private def namedInvocablesToTaskContext(namedInvocables: NamedInvocables) = TaskContext(
    JavaModule.spooler_log(namedInvocables),
    JavaModule.spooler_task(namedInvocables),
    JavaModule.spooler_job(namedInvocables),
    JavaModule.spooler(namedInvocables))

  final case class Arguments(val moduleFactory: ModuleFactory, reference: DotnetModuleReference) extends ModuleArguments
}
