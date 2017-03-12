package com.sos.jobscheduler.agent.scheduler.job.task

import akka.util.ByteString
import com.sos.jobscheduler.agent.data.commands.{StartNonApiTask, StartTask}
import com.sos.jobscheduler.agent.scheduler.job.JobConfiguration
import com.sos.jobscheduler.agent.task.{AgentTask, AgentTaskFactory}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.SIGKILL
import com.sos.jobscheduler.base.utils.ScalaUtils.cast
import com.sos.jobscheduler.common.scalautil.SetOnce
import com.sos.jobscheduler.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.OrderStepSucceeded
import com.sos.jobscheduler.minicom.remoting.ClientRemoting
import com.sos.jobscheduler.minicom.remoting.dialog.ClientDialogConnection
import com.sos.jobscheduler.minicom.remoting.proxy.ProxyIDispatch
import com.sos.jobscheduler.taskserver.task.RemoteModuleInstanceServer
import com.sos.jobscheduler.tunnel.server.TunnelHandle
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class TaskRunner(jobConfiguration: JobConfiguration, newTask: AgentTaskFactory)
  (implicit executionContext: ExecutionContext) {

  private val taskOnce = new SetOnce[AgentTask]
  private val moduleInstanceRunnerOnce = new SetOnce[ModuleInstanceRunner]
  private var _killed = false
  private val taskId = StartTask.Meta.NoCppJobSchedulerTaskId

  def processOrder(order: Order[Order.InProcess.type]): Future[OrderStepSucceeded] = {
    if (killed)
      Future.failed(newKilledException())
    else
      for ((moduleInstanceRunner, startOk) ← startedModuleInstanceRunner();
           orderStepSucceeded ←
             if (!startOk) throw new IllegalStateException("Task has refused to start (in spooler_init or spooler_open)")
             else if (killed) throw newKilledException()
             else moduleInstanceRunner.processOrder(order))
        yield orderStepSucceeded
  }

  private def startedModuleInstanceRunner(): Future[(ModuleInstanceRunner, Boolean)] =
    moduleInstanceRunnerOnce.toOption match {
      case None ⇒ startModuleInstance()
      case Some(o) ⇒ Future.successful((o, true))
    }

  private def startModuleInstance(): Future[(ModuleInstanceRunner, Boolean)] = {
    val task = startTask()
    taskOnce := task
    val remoting = newRemoting(task.tunnel, name = task.id.string)
    for (moduleInstance ← createModuleInstance(remoting);
         moduleInstanceRunner = moduleInstanceRunnerOnce := new ModuleInstanceRunner(jobConfiguration, taskId, moduleInstance);
         startOk ← moduleInstanceRunner.start())
      yield
        (moduleInstanceRunner, startOk)
  }

  private def startTask(): AgentTask = {
    val command = StartNonApiTask(Some(StartTask.Meta(job = jobConfiguration.path.string, taskId)))
    newTask(command, clientIpOption = None) sideEffect {
      _.start()
    }
  }

  def kill(signal: ProcessSignal): Unit = {
    for (task ← taskOnce) task.sendProcessSignal(signal)
    _killed |= signal == SIGKILL
  }

  private def newRemoting(tunnel: TunnelHandle, name: String): ClientRemoting =
    new ClientRemoting (
      new ClientDialogConnection with ClientDialogConnection.ImplementBlocking {
        protected implicit def executionContext = TaskRunner.this.executionContext
        def sendAndReceive(data: ByteString) =
          tunnel.request(data, timeout = None) map Some.apply
      },
      name = name)

  private def createModuleInstance(remoting: ClientRemoting): Future[ProxyIDispatch] =
    remoting.createInstance(RemoteModuleInstanceServer.clsid, RemoteModuleInstanceServer.iid) map
      cast[ProxyIDispatch]

  def terminate(): Future[Completed] = {
    val whenCompleted =
      moduleInstanceRunnerOnce.toOption match {
        case Some(moduleInstanceRunner) ⇒
          moduleInstanceRunner.terminate()
        case None ⇒
          Future.successful(Completed)
      }
    whenCompleted onComplete { _ ⇒
      for (o ← taskOnce) o.close()
    }
    whenCompleted
  }

  def killed: Boolean =
    _killed

  private def newKilledException() = new IllegalStateException("Task killed")
}

object TaskRunner {
  def stepOne(jobConfiguration: JobConfiguration, order: Order[Order.InProcess.type])(implicit newTask: AgentTaskFactory, ec: ExecutionContext)
  : Future[OrderStepSucceeded]
  = {
    val taskRunner = new TaskRunner(jobConfiguration, newTask)
    taskRunner.processOrder(order) andThen { case _ ⇒ taskRunner.terminate() }
  }
}
