package com.sos.scheduler.engine.taskserver.modules.shell

import com.sos.scheduler.engine.taskserver.data.{TaskServerArguments, TaskServerMainTerminated}
import com.sos.scheduler.engine.taskserver.moduleapi.{Module, ModuleArguments, ModuleFactory, ModuleLanguage, RawModuleArguments, Script}
import com.sos.scheduler.engine.taskserver.modules.common.CommonArguments
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Joacim Zschimmer
 */
final class ShellModule(
  arguments: ShellModule.Arguments,
  synchronizedStartProcess: RichProcessStartSynchronizer)
extends Module {

  def script: Script = arguments.script

  def newTask(
    commonArguments: CommonArguments,
    taskServerArguments: TaskServerArguments,
    environment: Map[String, String],
    shellVariablePrefix: String,
    synchronizedStartProcess: RichProcessStartSynchronizer,
    taskServerMainTerminatedOption: Option[Future[TaskServerMainTerminated.type]])
    (implicit ec: ExecutionContext)
  =
    new ShellProcessTask(
      this,
      commonArguments,
      environment = taskServerArguments.environment ++ environment,
      variablePrefix = shellVariablePrefix,
      logDirectory = taskServerArguments.logDirectory,
      logFilenamePart = taskServerArguments.logFilenamePart,
      killScriptOption = taskServerArguments.killScriptOption,
      synchronizedStartProcess,
      taskServerMainTerminatedOption = taskServerMainTerminatedOption)
}

object ShellModule {
  final case class Factory @Inject private[shell](synchronizedStartProcess: RichProcessStartSynchronizer)
  extends ModuleFactory {

    def toModuleArguments = {
      case RawModuleArguments(ModuleLanguage("shell"), None, script, None, None) ⇒
        Arguments(this, script)
    }

    def newModule(arguments: ModuleArguments) = new ShellModule(
      arguments.asInstanceOf[Arguments],
      synchronizedStartProcess)
  }

  final case class Arguments(moduleFactory: Factory, script: Script) extends ModuleArguments
}
