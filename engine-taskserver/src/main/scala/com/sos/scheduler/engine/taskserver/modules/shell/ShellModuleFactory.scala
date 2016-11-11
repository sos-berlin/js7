package com.sos.scheduler.engine.taskserver.modules.shell

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class ShellModuleFactory @Inject private(
  synchronizedStartProcess: RichProcessStartSynchronizer)
  (implicit ec: ExecutionContext)
{
  def apply(arguments: ShellModule.Arguments): ShellModule =
    new ShellModule(
      arguments,
      synchronizedStartProcess)
}
