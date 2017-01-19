package com.sos.scheduler.engine.taskserver.modules.dotnet

import com.sos.scheduler.engine.taskserver.dotnet.api.{DotnetModuleInstanceFactory, DotnetModuleReference, TaskContext}
import com.sos.scheduler.engine.taskserver.moduleapi.{ModuleArguments, ModuleFactory, ModuleLanguage, RawModuleArguments}
import com.sos.scheduler.engine.taskserver.modules.javamodule.{ApiModule, JavaModule}
import com.sos.scheduler.engine.taskserver.spoolerapi.TypedNamedIDispatches
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final class DotnetModule private[dotnet](val arguments: DotnetModule.Arguments, factory: DotnetModuleInstanceFactory)
extends ApiModule {
  import DotnetModule._

  def newJobInstance(namedIDispatches: TypedNamedIDispatches) =
    factory.newInstance(classOf[sos.spooler.IJob_impl], namedIDispatchesToTaskContext(namedIDispatches), arguments.reference)

  def newMonitorInstance(namedIDispatches: TypedNamedIDispatches) =
    factory.newInstance(classOf[sos.spooler.IMonitor_impl], namedIDispatchesToTaskContext(namedIDispatches), arguments.reference)
}

object DotnetModule {
  private val DotnetClassLanguage = ModuleLanguage("dotnet")
  private val PowershellLanguage = ModuleLanguage("powershell")

  private object ScriptControlLanguage {
    private val Prefix = "scriptcontrol:"
    def unapply(lang: ModuleLanguage) = if (lang.string startsWith Prefix) Some(lang.string stripPrefix Prefix) else None
  }

  final class Factory(factory: DotnetModuleInstanceFactory, classDllDirectory: Option[Path]) extends ModuleFactory {
    def toModuleArguments: PartialFunction[RawModuleArguments, ModuleArguments] = {
      case RawModuleArguments(PowershellLanguage, None, script, None, None) ⇒
        Arguments(this, DotnetModuleReference.Powershell(script = script.string))

      case RawModuleArguments(DotnetClassLanguage, None, _, Some(dll), Some(className)) ⇒
        val withDirectory = classDllDirectory map { _ resolve dll } getOrElse dll
        Arguments(this, DotnetModuleReference.DotnetClass(dll = withDirectory, className = className))

      case RawModuleArguments(ScriptControlLanguage(language), None, script, None, None) ⇒
        Arguments(this, DotnetModuleReference.ScriptControl(language = language, script = script.string))
    }

    def newModule(arguments: ModuleArguments) =
      new DotnetModule(arguments.asInstanceOf[Arguments], factory)

    override def toString = s"DotnetModule.Factory($factory)"
  }

  private def namedIDispatchesToTaskContext(namedIDispatches: TypedNamedIDispatches) = TaskContext(
    JavaModule.spooler_log(namedIDispatches),
    JavaModule.spooler_task(namedIDispatches),
    JavaModule.spooler_job(namedIDispatches),
    JavaModule.spooler(namedIDispatches))

  final case class Arguments(moduleFactory: ModuleFactory, reference: DotnetModuleReference) extends ModuleArguments
}
