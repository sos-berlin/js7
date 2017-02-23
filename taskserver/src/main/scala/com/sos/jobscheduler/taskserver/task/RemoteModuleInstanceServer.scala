package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.utils.ScalaUtils.cast
import com.sos.scheduler.engine.common.process.Processes.Pid
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger, SetOnce}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.jobapi.JavaJobSignatures.{SpoolerExitSignature, SpoolerOnErrorSignature}
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.idispatch.{AnnotatedInvocable, IDispatch, IUnknownFactory, InvocableIDispatch}
import com.sos.scheduler.engine.minicom.types.{CLSID, IID, VariantArray}
import com.sos.scheduler.engine.taskserver.common.StdFiles
import com.sos.scheduler.engine.taskserver.data.{TaskServerArguments, TaskServerMainTerminated}
import com.sos.scheduler.engine.taskserver.moduleapi.ModuleFactoryRegister
import com.sos.scheduler.engine.taskserver.modules.common.{CommonArguments, Task}
import com.sos.scheduler.engine.taskserver.modules.javamodule.{ApiModule, ApiProcessTask}
import com.sos.scheduler.engine.taskserver.modules.monitor.Monitor
import com.sos.scheduler.engine.taskserver.modules.shell.{RichProcessStartSynchronizer, ShellModule, ShellProcessTask}
import com.sos.scheduler.engine.taskserver.spoolerapi.TypedNamedIDispatches
import java.util.UUID
import javax.inject.{Inject, Singleton}
import org.scalactic.Requirements._
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Joacim Zschimmer
 * @see Com_remote_module_instance_server, spooler_module_remote_server.cxx
 */
final class RemoteModuleInstanceServer private(
  moduleFactoryRegister: ModuleFactoryRegister,
  taskServerArguments: TaskServerArguments,
  synchronizedStartProcess: RichProcessStartSynchronizer,
  taskServerMainTerminatedOption: Option[Future[TaskServerMainTerminated.type]])
  (implicit ec: ExecutionContext)
extends HasCloser with AnnotatedInvocable with InvocableIDispatch {
  import com.sos.scheduler.engine.taskserver.task.RemoteModuleInstanceServer._

  private var taskArguments: TaskArguments = null
  private val taskOnce = new SetOnce[Task]

  onClose {  // Called when Remoting releases this object (ReleaseCall)
    deleteLogFiles()
  }

  @invocable
  def construct(arguments: VariantArray): Unit = {
    taskArguments = TaskArguments(arguments)
    if (taskArguments.taskId != TaskId(0)) {  // C++ JobScheduler ?
      logger.info(s"${taskServerArguments.agentTaskId} is Master's task ${taskArguments.jobName}:${taskArguments.taskId}")
    }
  }

  @invocable
  def begin(objectAnys: VariantArray, objectNamesAnys: VariantArray): Boolean = {
    val namedIDispatches = toNamedObjectMap(names = objectNamesAnys, anys = objectAnys)
    val stdFiles = StdFiles(
      stdFileMap = taskServerArguments.stdFileMap filter { _ ⇒ taskServerArguments.logStdoutAndStderr },
      stderrLogLevel = taskArguments.stderrLogLevel,
      log = namedIDispatches.spoolerLog.log
    )
    val commonArguments = CommonArguments(
      taskServerArguments.agentTaskId,
      jobName = taskArguments.jobName,
      namedIDispatches,
      taskArguments.rawMonitorArguments map { o ⇒ Monitor(moduleFactoryRegister.toModuleArguments(o.rawModuleArguments), o.name, o.ordering) },
      hasOrder = taskArguments.hasOrder,
      stdFiles)
    val task = moduleFactoryRegister.newModule(taskArguments.rawModuleArguments) match {
      case module: ShellModule ⇒
        module.newTask(
          commonArguments,
          taskServerArguments,
          environment = taskArguments.environment,
          shellVariablePrefix = taskArguments.shellVariablePrefix,
          synchronizedStartProcess,
          taskServerMainTerminatedOption = taskServerMainTerminatedOption)
      case module: ApiModule ⇒
        new ApiProcessTask(module, commonArguments)
    }
    closer.registerAutoCloseable(task)
    taskOnce := task
    task.start() await 1.h   // Asynchronous @invocable would be nice ...
  }

  @invocable
  def end(succeeded: Boolean): Unit =
    for (t ← taskOnce) t.end()

  @invocable
  def step(): Any = taskOnce().step()

  @invocable
  def call(javaSignature: String): Any = {
    if (taskOnce.isEmpty && Set(SpoolerOnErrorSignature, SpoolerExitSignature)(javaSignature))
      ()
    else
      taskOnce().callIfExists(javaSignature)
  }

  @invocable
  def waitForSubprocesses(): Unit = {}

  def sendProcessSignal(signal: ProcessSignal): Unit =
    taskOnce.toOption match {
      case Some(o: ShellProcessTask) ⇒ o.sendProcessSignal(signal)
      case _ ⇒
    }

  private def deleteLogFiles(): Unit =
    taskOnce.toOption match {
      case Some(o: ShellProcessTask) ⇒ o.deleteLogFiles()
      case _ ⇒
    }

  override def toString = List(
      s"${getClass.getSimpleName}",
      Option(taskArguments) map { t ⇒ s"(task ${t.jobName}:${t.taskId.string})" } getOrElse ""
    ).mkString("")

  def pidOption: Option[Pid] = taskOnce flatMap { _.pidOption }
}

object RemoteModuleInstanceServer {
  val clsid = CLSID(UUID fromString "feee47a3-6c1b-11d8-8103-000476ee8afb")
  val iid   = IID  (UUID fromString "feee47a2-6c1b-11d8-8103-000476ee8afb")

  private val logger = Logger(getClass)

  private def toNamedObjectMap(names: VariantArray, anys: VariantArray): TypedNamedIDispatches = {
    val nameStrings = names.as[String]
    val iDispatches = variantArrayToIDispatches(anys)
    require(nameStrings.size == iDispatches.size)
    TypedNamedIDispatches(nameStrings zip iDispatches)
  }

  /**
   * Expects an VariantArray with Some[IUnknown]
   *
   * @return IUnknown, interpreted as Invocable
   * @throws NullPointerException when an IUnknown is null.
   */
  private def variantArrayToIDispatches(a: VariantArray) = a.indexedSeq map cast[IDispatch]

  trait MyIUnknownFactory extends IUnknownFactory {
    final val clsid = RemoteModuleInstanceServer.this.clsid
    final val iid   = RemoteModuleInstanceServer.this.iid
  }

  @Singleton
  final class Factory @Inject private(
    moduleFactoryRegister: ModuleFactoryRegister,
    synchronizedStartProcess: RichProcessStartSynchronizer,
    taskServerMainTerminatedOption: Option[Future[TaskServerMainTerminated.type]])
    (implicit ec: ExecutionContext)
  extends (TaskServerArguments ⇒ RemoteModuleInstanceServer)
  {
    def apply(taskServerArguments: TaskServerArguments): RemoteModuleInstanceServer =
      new RemoteModuleInstanceServer(
        moduleFactoryRegister = moduleFactoryRegister,
        taskServerArguments = taskServerArguments,
        synchronizedStartProcess = synchronizedStartProcess,
        taskServerMainTerminatedOption = taskServerMainTerminatedOption)
  }
}
