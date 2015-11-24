package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversableOnce
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.cast
import com.sos.scheduler.engine.data.jobapi.JavaJobSignatures.{SpoolerExitSignature, SpoolerOnErrorSignature}
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.idispatch.{Invocable, InvocableFactory}
import com.sos.scheduler.engine.minicom.types.{CLSID, IID, VariantArray}
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import com.sos.scheduler.engine.taskserver.module.NamedInvocables
import com.sos.scheduler.engine.taskserver.module.javamodule.JavaModule
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import com.sos.scheduler.engine.taskserver.task.process.Processes.Pid
import java.util.UUID
import javax.inject.Inject
import org.scalactic.Requirements._
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 * @see Com_remote_module_instance_server, spooler_module_remote_server.cxx
 */
final class RemoteModuleInstanceServer @Inject private(taskStartArguments: TaskStartArguments, taskServerMainTerminatedOption: Option[Future[Unit]])
extends HasCloser with Invocable {
  import com.sos.scheduler.engine.taskserver.task.RemoteModuleInstanceServer._

  private var taskArguments: TaskArguments = null
  private var task: Task = null

  @invocable
  def construct(arguments: VariantArray): Unit = taskArguments = TaskArguments(arguments)

  @invocable
  def begin(objectAnys: VariantArray, objectNamesAnys: VariantArray): Boolean = {
    val namedInvocables = toNamedObjectMap(names = objectNamesAnys, anys = objectAnys)
    val stdFiles = StdFiles(
      stdFileMap = taskStartArguments.stdFileMap filter { _ ⇒ taskStartArguments.logStdoutAndStderr },
      stderrLogLevel = taskArguments.stderrLogLevel,
      log = namedInvocables.spoolerLog.log
    )
    val commonArguments = CommonArguments(
      taskStartArguments.agentTaskId,
      jobName = taskArguments.jobName,
      namedInvocables,
      taskArguments.monitors,
      hasOrder = taskArguments.hasOrder,
      stdFiles)
    task = taskArguments.module match {
      case module: ShellModule ⇒
        new ShellProcessTask(module, commonArguments,
          environment = taskStartArguments.environment.toImmutableSeq ++ taskArguments.environment,
          variablePrefix = taskArguments.shellVariablePrefix,
          logDirectory = taskStartArguments.logDirectory,
          logFilenamePart = taskStartArguments.logFilenamePart,
          killScriptPathOption = taskStartArguments.killScriptFileOption,
          taskServerMainTerminatedOption = taskServerMainTerminatedOption)
      case module: JavaModule ⇒
        new JavaProcessTask(module, commonArguments)
    }
    closer.registerAutoCloseable(task)
    task.start()
  }

  @invocable
  def end(succeeded: Boolean): Unit = {
    if (task != null)
      task.end()
  }

  @invocable
  def step(): Any = task.step()

  @invocable
  def call(javaSignature: String): Any = {
    if (task == null && Set(SpoolerOnErrorSignature, SpoolerExitSignature)(javaSignature))
      ()
    else {
      require(task != null, s"No task when calling $javaSignature")
      task.callIfExists(javaSignature)
    }
  }

  @invocable
  def waitForSubprocesses(): Unit = {}

  def sendProcessSignal(signal: ProcessSignal): Unit =
    task match {
      case o: ShellProcessTask ⇒ o.sendProcessSignal(signal)
    }

  override def toString = List(
      s"${getClass.getSimpleName}",
      Option(taskArguments) map { t ⇒ s"(task ${t.jobName}:${t.taskId.string})" } getOrElse ""
    ).mkString("")

  def pidOption: Option[Pid] = Option(task) flatMap { _.pidOption }
}

object RemoteModuleInstanceServer extends InvocableFactory {
  val clsid = CLSID(UUID fromString "feee47a3-6c1b-11d8-8103-000476ee8afb")
  val iid   = IID  (UUID fromString "feee47a2-6c1b-11d8-8103-000476ee8afb")

  def invocableClass = classOf[RemoteModuleInstanceServer]

  private def toNamedObjectMap(names: VariantArray, anys: VariantArray): NamedInvocables = {
    val nameStrings = names.as[String]
    val invocables = variantArrayToInvocable(anys)
    require(nameStrings.size == invocables.size)
    NamedInvocables(nameStrings zip invocables)
  }

  /**
   * Expects an VariantArray with Some[IUnknown]
   * @return IUnknown, interpreted as Invocable
   * @throws NullPointerException when an IUnknown is null.
   */
  private def variantArrayToInvocable(a: VariantArray) = a.indexedSeq map cast[Invocable]
}
