package com.sos.jobscheduler.agent.scheduler.job.task

import akka.util.ByteString
import com.sos.jobscheduler.agent.scheduler.job.JobConfiguration
import com.sos.jobscheduler.agent.scheduler.job.task.LegacyApiTaskRunner._
import com.sos.jobscheduler.agent.task.{AgentTask, AgentTaskFactory, StartNonApiTask}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.SIGKILL
import com.sos.jobscheduler.base.utils.ScalaUtils.cast
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.data.job.TaskId
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.minicom.remoting.ClientRemoting
import com.sos.jobscheduler.minicom.remoting.dialog.ClientDialogConnection
import com.sos.jobscheduler.minicom.remoting.proxy.ProxyIDispatch
import com.sos.jobscheduler.taskserver.task.RemoteModuleInstanceServer
import com.sos.jobscheduler.taskserver.task.process.StdChannels
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class LegacyApiTaskRunner private[task](jobConfiguration: JobConfiguration, newTask: AgentTaskFactory)
  (implicit executionContext: ExecutionContext)
extends TaskRunner {

  def asBaseAgentTask = taskOnce()

  private val taskOnce = new SetOnce[AgentTask]
  private val moduleInstanceRunnerOnce = new SetOnce[ModuleInstanceRunner]
  private var _killed = false
  private val taskId = TaskId(-1) // ???

  def processOrder(order: Order[Order.InProcess.type], stdChannels: StdChannels) = {
    // TODO stdoutStderrHandle is not used. No OrderStdWritten events are produced. Maybe LegacyApiTaskRunner will not be used anyway (requirement?).
    if (killed)
      Future.failed(newKilledException())
    else
      for ((moduleInstanceRunner, startOk) ← startedModuleInstanceRunner();
           moduleStepEnded ←
             if (!startOk) throw new IllegalStateException("Task has refused to start (in spooler_init or spooler_open)")
             else if (killed) throw newKilledException()
             else moduleInstanceRunner.processOrder(order))
        yield moduleStepEnded
  }

  private def startedModuleInstanceRunner(): Future[(ModuleInstanceRunner, Boolean)] =
    moduleInstanceRunnerOnce.toOption match {
      case None ⇒ startModuleInstance()
      case Some(o) ⇒ Future.successful((o, true))
    }

  private def startModuleInstance(): Future[(ModuleInstanceRunner, Boolean)] = {
    val task = startTask()
    taskOnce := task
    val remoting = newRemoting(task.request, name = task.id.string)
    for (moduleInstance ← createModuleInstance(remoting);
         moduleInstanceRunner = moduleInstanceRunnerOnce := new ModuleInstanceRunner(jobConfiguration, taskId, moduleInstance);
         startOk ← moduleInstanceRunner.start())
      yield
        (moduleInstanceRunner, startOk)
  }

  private def startTask(): AgentTask = {
    val command = StartNonApiTask(jobPath = jobConfiguration.path)
    val task = newTask(command, clientIpOption = None)
    task.start()
    task
  }

  def kill(signal: ProcessSignal): Unit = {
    logger.debug(s"Sending $signal to $toString")
    for (task ← taskOnce) task.sendProcessSignal(signal)
    _killed |= signal == SIGKILL
  }

  private def newRemoting(request: ByteString ⇒ Future[ByteString], name: String): ClientRemoting =
    new ClientRemoting (
      new ClientDialogConnection with ClientDialogConnection.ImplementBlocking {
        protected implicit def executionContext = LegacyApiTaskRunner.this.executionContext
        def sendAndReceive(data: ByteString) = request(data) map Some.apply
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

  override def toString =
    s"LegacyApiTaskRunner(${jobConfiguration.path}" +
      (taskOnce.toOption map { _.toString } getOrElse "") +
      ")"
}

object LegacyApiTaskRunner {
  private val logger = Logger(getClass)

  @Singleton
  final case class Factory @Inject private(newTask: AgentTaskFactory)(implicit ec: ExecutionContext) extends TaskRunner.Factory {
    def apply(jobConfiguration: JobConfiguration) =
      new LegacyApiTaskRunner(jobConfiguration, newTask)
  }
}
