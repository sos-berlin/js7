package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.taskserver.task.StartConfiguration

/**
 * @author Joacim Zschimmer
 */
object RemoteTaskFactory {

  def apply(conf: StartConfiguration): RemoteTask = {
    if (conf.usesApi) {
      throw new UnsupportedOperationException("Remote API tasks are not yet implemented")
      //          val task = new DedicatedProcessRemoteTask(remoteTaskId, startConfiguration)
      //          task.start()
      // Prozess starten (java -classpath ... -controller=x.x.x.x/nnn)
    } else
      new InProcessRemoteTask(conf)
  }
}
