package com.sos.scheduler.engine.taskserver.modules.shell

import com.sos.scheduler.engine.taskserver.TaskServerMain
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class ShellModuleFactory @Inject private(
  synchronizedStartProcess: RichProcessStartSynchronizer,
  taskServerMainTerminatedOption: Option[Future[TaskServerMain.Terminated.type]])
  (implicit ec: ExecutionContext)
{
  def apply(arguments: ShellModule.Arguments): ShellModule =
    new ShellModule(
      arguments,
      synchronizedStartProcess,
      taskServerMainTerminatedOption = taskServerMainTerminatedOption)
}
