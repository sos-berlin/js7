package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.scheduler.job.JobConfiguration
import com.sos.jobscheduler.agent.scheduler.job.task.SimpleShellTaskRunner._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.log.LazyScalaLogger.AsLazyScalaLogger
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.utils.Exceptions.logException
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.taskserver.modules.shell.RichProcessStartSynchronizer
import com.sos.jobscheduler.taskserver.task.TaskArguments
import com.sos.jobscheduler.taskserver.task.process.ShellScriptProcess.startPipedShellScript
import com.sos.jobscheduler.taskserver.task.process.{ProcessConfiguration, RichProcess, StdoutStderrWriter}
import java.nio.file.Files.delete
import javax.inject.{Inject, Singleton}
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
  private lazy val returnValuesProvider = new ShellReturnValuesProvider
  private val richProcessOnce = new SetOnce[RichProcess]

  def terminate(): Future[Completed] =
    richProcessOnce.toOption match {
      case Some(richProcess) ⇒
        richProcess.terminated.mapTo[Completed.type]
      case None ⇒
        Future.successful(Completed)
    }

  def processOrder(order: Order[Order.InProcess.type], stdoutStderrWriter: StdoutStderrWriter): Future[TaskStepEnded] =
    for (returnCode ← runProcess(order, stdoutStderrWriter)) yield
      TaskStepSucceeded(
        MapDiff.diff(order.variables, order.variables ++ fetchReturnValuesThenDeleteFile()),
        Order.Good(returnCode.isSuccess))

  private def runProcess(order: Order[Order.InProcess.type], stdoutStderrWriter: StdoutStderrWriter): Future[ReturnCode] =
    for {
      richProcess ← startProcess(order, stdoutStderrWriter) andThen {
        case Success(richProcess) ⇒ logger.info(s"Process '$richProcess' started for ${order.id}, ${jobConfiguration.path}")
      }
      returnCode ← richProcess.terminated andThen { case tried ⇒
        logger.info(s"Process '$richProcess' terminated with $tried")
      }
    } yield {
      richProcess.close()
      returnCode
    }

  private def fetchReturnValuesThenDeleteFile(): Map[String, String] = {
    val result = returnValuesProvider.variables
    logException(logger.asLazy.error) {    // TODO When Windows locks the file, try delete it later, asynchronously
      delete(returnValuesProvider.file)
    }
    result
  }

  private def startProcess(order: Order[Order.InProcess.type], stdoutStderrWriter: StdoutStderrWriter): Future[RichProcess] = {
    val env = {
      val params = jobConfiguration.variables ++ order.variables
      val paramEnv = params map { case (k, v) ⇒ (variablePrefix concat k.toUpperCase) → v }
      /*environment +*/ paramEnv + returnValuesProvider.env
    }
    val processConfiguration = ProcessConfiguration(
      stdFileMap = Map(),
      additionalEnvironment = env,
      agentTaskIdOption = Some(agentTaskId),
      killScriptOption = agentConfiguration.killScript)
    synchronizedStartProcess {
      startPipedShellScript(
        processConfiguration,
        name = jobConfiguration.path.name,
        script = jobConfiguration.script.string.trim,
        stdoutStderrWriter)
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
  final class Factory @Inject()(
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
