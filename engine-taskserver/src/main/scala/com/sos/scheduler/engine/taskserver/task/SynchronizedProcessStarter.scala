package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.async.synchronizer.ActorSynchronizer
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class SynchronizedProcessStarter(synchronizer: ActorSynchronizer[Process])  {
  def apply(processBuilder: ProcessBuilder): Future[Process] = synchronizer { processBuilder.start() }
}
