package com.sos.scheduler.engine.agent.orderprocessing.job.task

import akka.util.ByteString
import com.sos.scheduler.engine.agent.data.commands.{StartNonApiTask, StartTask}
import com.sos.scheduler.engine.agent.orderprocessing.job.JobConfiguration
import com.sos.scheduler.engine.agent.task.{AgentTask, AgentTaskFactory}
import com.sos.scheduler.engine.base.generic.Completed
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal.SIGKILL
import com.sos.scheduler.engine.base.utils.ScalaUtils.cast
import com.sos.scheduler.engine.common.scalautil.SetOnce
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.data.engine2.order.Order
import com.sos.scheduler.engine.data.engine2.order.OrderEvent.OrderStepSucceeded
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.minicom.remoting.ClientRemoting
import com.sos.scheduler.engine.minicom.remoting.dialog.ClientDialogConnection
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyIDispatch
import com.sos.scheduler.engine.taskserver.task.RemoteModuleInstanceServer
import com.sos.scheduler.engine.tunnel.server.TunnelHandle
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class TaskRunner(jobConfiguration: JobConfiguration, taskId: TaskId, newTask: AgentTaskFactory)
  (implicit ec: ExecutionContext) {

  private val taskOnce = new SetOnce[AgentTask]
  private val moduleInstanceRunnerOnce = new SetOnce[ModuleInstanceRunner]
  private var _killed = false

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
    new ClientRemoting(
      new ClientDialogConnection {
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
    val taskRunner = new TaskRunner(jobConfiguration, TaskId(0), newTask)
    taskRunner.processOrder(order) andThen { case _ ⇒ taskRunner.terminate() }
  }
}
