package com.sos.scheduler.engine.taskserver.modules.shell

import com.sos.scheduler.engine.taskserver.TaskServerMain
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
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
  def script = arguments.script

  def newTask(
    commonArguments: CommonArguments,
    taskStartArguments: TaskStartArguments,
    environment: Map[String, String],
    shellVariablePrefix: String,
    synchronizedStartProcess: RichProcessStartSynchronizer,
    taskServerMainTerminatedOption: Option[Future[TaskServerMain.Terminated.type]])
    (implicit ec: ExecutionContext)
  =
    new ShellProcessTask(
      this,
      commonArguments,
      environment = taskStartArguments.environment ++ environment,
      variablePrefix = shellVariablePrefix,
      logDirectory = taskStartArguments.logDirectory,
      logFilenamePart = taskStartArguments.logFilenamePart,
      killScriptOption = taskStartArguments.killScriptOption,
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
