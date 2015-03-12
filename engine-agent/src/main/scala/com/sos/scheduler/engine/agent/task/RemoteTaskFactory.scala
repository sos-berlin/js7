package com.sos.scheduler.engine.agent.task

/**
 * @author Joacim Zschimmer
 */
object RemoteTaskFactory extends (RemoteTaskFactoryArguments ⇒ RemoteTask) {

  def apply(arguments: RemoteTaskFactoryArguments): RemoteTask = arguments match {
    case InProcessRemoteTaskFactoryArguments(args) ⇒ new InProcessRemoteTask(args)
    case o: DedicatedProcessRemoteTaskFactoryArguments ⇒ new DedicatedProcessRemoteTask(o)
  }
}
