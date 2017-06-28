package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.jobscheduler.agent.scheduler.job.JobConfiguration
import com.sos.jobscheduler.agent.scheduler.job.task.SimpleShellTaskRunner._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.log.LazyScalaLogger.AsLazyScalaLogger
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.utils.Exceptions.logException
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.taskserver.modules.shell.RichProcessStartSynchronizer
import com.sos.jobscheduler.taskserver.task.TaskArguments
import com.sos.jobscheduler.taskserver.task.process.ShellScriptProcess.startShellScript
import com.sos.jobscheduler.taskserver.task.process.{ProcessConfiguration, RichProcess}
import java.nio.file.Files.delete
import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

/**
  * @author Joacim Zschimmer
  */
final class SimpleShellTaskRunner(jobConfiguration: JobConfiguration,
  agentTaskId: AgentTaskId,
  synchronizedStartProcess: RichProcessStartSynchronizer,
  agentConfiguration: AgentConfiguration)
  (implicit ec: ExecutionContext)
extends TaskRunner {

  private val variablePrefix = TaskArguments.DefaultShellVariablePrefix
  private val stdFileMap = RichProcess.createStdFiles(agentConfiguration.logDirectory, id = agentTaskId.toString)
  private lazy val returnValuesProvider = new ShellReturnValuesProvider
  private val richProcessOnce = new SetOnce[RichProcess]

  def terminate(): Future[Completed] =
    richProcessOnce.toOption match {
      case Some(richProcess) ⇒
        richProcess.terminated.mapTo[Completed.type]
      case None ⇒
        Future.successful(Completed)
    }

  def processOrder(order: Order[Order.InProcess.type]): Future[TaskStepEnded] =
    for {
      richProcess ← startProcess(order)
      returnCode ← richProcess.terminated
    } yield {
      richProcess.close()
      val newVariables = order.variables ++ returnValuesProvider.variables
      logException(logger.asLazy.error) {
        delete(returnValuesProvider.file)
        RichProcess.tryDeleteFiles(stdFileMap.values)
      }
      TaskStepSucceeded(
        MapDiff.diff(order.variables, newVariables),
        Order.Good(returnCode.isSuccess))
    }

  private def startProcess(order: Order[Order.InProcess.type]): Future[RichProcess] = {
    val env = {
      val params = jobConfiguration.variables ++ order.variables
      val paramEnv = params map { case (k, v) ⇒ (variablePrefix concat k.toUpperCase) → v }
      /*environment +*/ paramEnv + returnValuesProvider.env
    }
    val processConfiguration = ProcessConfiguration(
      stdFileMap,
      additionalEnvironment = env,
      agentTaskIdOption = Some(agentTaskId),
      killScriptOption = agentConfiguration.killScript)
    synchronizedStartProcess {
      startShellScript(
        processConfiguration,
        name = jobConfiguration.path.name,
        scriptString = jobConfiguration.script.string.trim)
    } andThen { case Success(richProcess) ⇒
      richProcessOnce := richProcess
    }
  }

  def kill(signal: ProcessSignal): Unit = {
    logger.trace(s"sendProcessSignal $signal")
    for (p ← richProcessOnce) p.sendProcessSignal(signal)
  }
}

object SimpleShellTaskRunner {
  private val logger = Logger(getClass)

  @Singleton
  final case class Factory @Inject private(
    agentTaskIdGenerator: AgentTaskId.Generator,
    synchronizedStartProcess: RichProcessStartSynchronizer,
    agentConfiguration: AgentConfiguration)
    (implicit ec: ExecutionContext)
  extends TaskRunner.Factory
  {
    def apply(jobConfiguration: JobConfiguration) = {
      val agentTaskId = agentTaskIdGenerator.next()
      new SimpleShellTaskRunner(jobConfiguration, agentTaskId, synchronizedStartProcess, agentConfiguration)
    }
  }
}
