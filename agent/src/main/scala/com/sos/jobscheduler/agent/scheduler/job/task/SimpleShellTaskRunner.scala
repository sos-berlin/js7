package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.TaskOverview
import com.sos.jobscheduler.agent.scheduler.job.ShellReturnValuesProvider
import com.sos.jobscheduler.agent.scheduler.job.task.SimpleShellTaskRunner._
import com.sos.jobscheduler.agent.task.BaseAgentTask
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.log.LazyScalaLogger.AsLazyScalaLogger
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.Exceptions.logException
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.taskserver.modules.shell.RichProcessStartSynchronizer
import com.sos.jobscheduler.taskserver.task.TaskArguments
import com.sos.jobscheduler.taskserver.task.process.ShellScriptProcess.startPipedShellScript
import com.sos.jobscheduler.taskserver.task.process.{ProcessConfiguration, RichProcess, StdChannels}
import java.nio.file.Files.delete
import java.time.Instant.now
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Success

/**
  * @author Joacim Zschimmer
  */
final class SimpleShellTaskRunner(conf: TaskConfiguration,
  agentTaskId: AgentTaskId,
  synchronizedStartProcess: RichProcessStartSynchronizer,
  agentConfiguration: AgentConfiguration)
  (implicit ec: ExecutionContext)
extends TaskRunner {

  val asBaseAgentTask = new BaseAgentTask {
    def id = agentTaskId
    def jobPath = conf.jobPath
    def pidOption = richProcessOnce flatMap { _.pidOption }
    def terminated = terminatedPromise.future
    def overview = TaskOverview(jobPath, id, pidOption, startedAt.toTimestamp)

    def sendProcessSignal(signal: ProcessSignal) =
      for (o ← richProcessOnce) o.sendProcessSignal(signal)

    override def toString = SimpleShellTaskRunner.this.toString
  }

  private val terminatedPromise = Promise[Completed]()
  private val startedAt = now
  private val variablePrefix = TaskArguments.DefaultShellVariablePrefix
  private lazy val returnValuesProvider = new ShellReturnValuesProvider
  private val richProcessOnce = new SetOnce[RichProcess]
  private var killedBeforeStart = false

  def terminate(): Future[Completed] =
    richProcessOnce.toOption match {
      case Some(richProcess) ⇒
        richProcess.terminated.mapTo[Completed.type]
      case None ⇒
        Future.successful(Completed)
    }

  def processOrder(order: Order[Order.InProcess.type], stdChannels: StdChannels): Future[TaskStepEnded] =
    for (returnCode ← runProcess(order, stdChannels)) yield
      TaskStepSucceeded(
        MapDiff.diff(order.variables, order.variables ++ fetchReturnValuesThenDeleteFile()),
        Outcome.Good(returnCode.isSuccess))

  private def runProcess(order: Order[Order.InProcess.type], stdChannels: StdChannels): Future[ReturnCode] =
    for {
      richProcess ← startProcess(order, stdChannels) andThen {
        case Success(richProcess) ⇒ logger.info(s"System process '$richProcess' started for ${order.id}, ${conf.jobPath}, script ${conf.shellFile}")
      }
      returnCode ← richProcess.terminated andThen { case tried ⇒
        logger.info(s"System process '$richProcess' terminated with $tried after ${richProcess.duration.pretty}")
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

  private def startProcess(order: Order[Order.InProcess.type], stdChannels: StdChannels): Future[RichProcess] = {
    if (killedBeforeStart)
      Future.failed(new RuntimeException(s"$agentTaskId killed before start"))
    else {
      val env = {
        val params = conf.jobConfiguration.variables ++ order.variables
        val paramEnv = params map { case (k, v) ⇒ (variablePrefix concat k.toUpperCase) → v }
        /*environment +*/ paramEnv + returnValuesProvider.env
      }
      val processConfiguration = ProcessConfiguration(
        stdFileMap = Map(),
        encoding = AgentConfiguration.FileEncoding,
        additionalEnvironment = env,
        agentTaskIdOption = Some(agentTaskId),
        killScriptOption = agentConfiguration.killScript)
      synchronizedStartProcess {
        startPipedShellScript(conf.shellFile, processConfiguration, stdChannels)
      } andThen { case Success(richProcess) ⇒
        terminatedPromise.completeWith(richProcess.terminated map { _ ⇒ Completed })
        richProcessOnce := richProcess
      }
    }
  }

  def kill(signal: ProcessSignal): Unit =
    richProcessOnce.toOption match {
      case Some(richProcess) ⇒
        richProcess.sendProcessSignal(signal)
      case None ⇒
        terminatedPromise.tryFailure(new RuntimeException(s"$agentTaskId killed before start"))
        killedBeforeStart = true
    }

  override def toString = s"SimpleShellTaskRunner($agentTaskId ${conf.jobPath})"
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
    def apply(conf: TaskConfiguration) = {
      val agentTaskId = agentTaskIdGenerator.next()
      Future.successful(new SimpleShellTaskRunner(conf, agentTaskId, synchronizedStartProcess, agentConfiguration))
    }
  }
}
