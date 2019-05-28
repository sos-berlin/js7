package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.TaskOverview
import com.sos.jobscheduler.agent.scheduler.job.ShellReturnValuesProvider
import com.sos.jobscheduler.agent.scheduler.job.task.SimpleShellTaskRunner._
import com.sos.jobscheduler.agent.task.BaseAgentTask
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.{IOExecutor, Logger, SetOnce}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.taskserver.modules.shell.RichProcessStartSynchronizer
import com.sos.jobscheduler.taskserver.task.process.ShellScriptProcess.startPipedShellScript
import com.sos.jobscheduler.taskserver.task.process.{ProcessConfiguration, RichProcess, StdChannels}
import java.nio.file.Files.delete
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Success
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class SimpleShellTaskRunner(conf: TaskConfiguration,
  agentTaskId: AgentTaskId,
  synchronizedStartProcess: RichProcessStartSynchronizer,
  agentConfiguration: AgentConfiguration)
  (implicit iox: IOExecutor, ec: ExecutionContext)
extends TaskRunner {

  val asBaseAgentTask = new BaseAgentTask {
    def id = agentTaskId
    def jobKey = conf.jobKey
    def pidOption = richProcessOnce flatMap { _.pidOption }
    def terminated = terminatedPromise.future
    def overview = TaskOverview(jobKey, id, pidOption, startedAt)

    def sendProcessSignal(signal: ProcessSignal) =
      for (o <- richProcessOnce) o.sendProcessSignal(signal)

    override def toString = SimpleShellTaskRunner.this.toString
  }

  private val terminatedPromise = Promise[Completed]()
  private val startedAt = Timestamp.now
  private val variablePrefix = DefaultShellVariablePrefix
  private lazy val returnValuesProvider = new ShellReturnValuesProvider(temporaryDirectory = agentConfiguration.temporaryDirectory)
  private val richProcessOnce = SetOnce[RichProcess]
  private var killedBeforeStart = false

  def terminate(): Future[Completed] =
    richProcessOnce.toOption match {
      case Some(richProcess) =>
        richProcess.terminated map (_ => Completed)
      case None =>
        Future.successful(Completed)
    }

  def processOrder(order: Order[Order.Processing], defaultArguments: Map[String, String], stdChannels: StdChannels): Future[TaskStepEnded] =
    for (returnCode <- runProcess(order, defaultArguments, stdChannels)) yield
      TaskStepSucceeded(fetchReturnValuesThenDeleteFile(), returnCode)

  private def runProcess(order: Order[Order.Processing], defaultArguments: Map[String, String], stdChannels: StdChannels): Future[ReturnCode] =
    for {
      richProcess <- startProcess(order, defaultArguments, stdChannels) andThen {
        case Success(richProcess) => logger.info(s"Process '$richProcess' started for ${order.id}, ${conf.jobKey}, script ${conf.shellFile}")
      }
      returnCode <- richProcess.terminated andThen { case tried =>
        logger.info(s"Process '$richProcess' terminated with ${tried getOrElse tried} after ${richProcess.duration.pretty}")
      }
    } yield {
      richProcess.close()
      returnCode
    }

  private def fetchReturnValuesThenDeleteFile(): Map[String, String] = {
    val result = returnValuesProvider.variables
    // TODO When Windows locks the file, try delete it later, asynchronously, and block file in FilePool
    try delete(returnValuesProvider.file)
    catch { case NonFatal(t) =>
      logger.error(s"Cannot delete file '${returnValuesProvider.file}': ${t.toStringWithCauses}")
      throw t
    }
    result
  }

  private def startProcess(order: Order[Order.Processing], defaultArguments: Map[String, String], stdChannels: StdChannels): Future[RichProcess] = {
    if (killedBeforeStart)
      Future.failed(new RuntimeException(s"$agentTaskId killed before start"))
    else {
      val env = {
        val params = conf.workflowJob.defaultArguments ++ defaultArguments ++ order.keyValues
        val paramEnv = params map { case (k, v) => (variablePrefix + k.toUpperCase) -> v }
        paramEnv + returnValuesProvider.env
      }
      val processConfiguration = ProcessConfiguration(
        stdFileMap = Map.empty,
        encoding = AgentConfiguration.FileEncoding,
        workingDirectory = Some(agentConfiguration.jobWorkingDirectory),
        additionalEnvironment = env,
        agentTaskIdOption = Some(agentTaskId),
        killScriptOption = agentConfiguration.killScript)
      synchronizedStartProcess {
        startPipedShellScript(conf.shellFile, processConfiguration, stdChannels)
      } andThen { case Success(richProcess) =>
        terminatedPromise.completeWith(richProcess.terminated map { _ => Completed })
        richProcessOnce := richProcess
      }
    }
  }

  def kill(signal: ProcessSignal): Unit =
    richProcessOnce.toOption match {
      case Some(richProcess) =>
        richProcess.sendProcessSignal(signal)
      case None =>
        terminatedPromise.tryFailure(new RuntimeException(s"$agentTaskId killed before start"))
        killedBeforeStart = true
    }

  override def toString = s"SimpleShellTaskRunner($agentTaskId ${conf.jobKey})"
}

object SimpleShellTaskRunner {
  private val DefaultShellVariablePrefix = "SCHEDULER_PARAM_"
  private val logger = Logger(getClass)

  @Singleton
  final class Factory @Inject()(
    agentTaskIdGenerator: AgentTaskId.Generator,
    synchronizedStartProcess: RichProcessStartSynchronizer,
    agentConfiguration: AgentConfiguration)
    (implicit iox: IOExecutor, ec: ExecutionContext)
  extends TaskRunner.Factory
  {
    def apply(conf: TaskConfiguration) = {
      val agentTaskId = agentTaskIdGenerator.next()
      new SimpleShellTaskRunner(conf, agentTaskId, synchronizedStartProcess, agentConfiguration)
    }
  }
}
