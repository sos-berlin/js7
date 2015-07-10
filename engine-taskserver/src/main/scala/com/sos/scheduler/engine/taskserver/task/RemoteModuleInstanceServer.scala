package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversableOnce
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.cast
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.idispatch.{Invocable, InvocableFactory}
import com.sos.scheduler.engine.minicom.types.{CLSID, IID, VariantArray}
import com.sos.scheduler.engine.taskserver.HasSendProcessSignal
import java.util.UUID
import com.sos.scheduler.engine.taskserver.module._
import com.sos.scheduler.engine.taskserver.module.java.JavaModule
import com.sos.scheduler.engine.taskserver.module.java.JavaModule.{SpoolerExitSignature, SpoolerOnErrorSignature}
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import javax.inject.Inject
import org.scalactic.Requirements._

/**
 * @author Joacim Zschimmer
 * @see Com_remote_module_instance_server, spooler_module_remote_server.cxx
 */
final class RemoteModuleInstanceServer @Inject private(taskStartArguments: TaskStartArguments)
extends HasCloser with Invocable with HasSendProcessSignal {
  import com.sos.scheduler.engine.taskserver.task.RemoteModuleInstanceServer._

  private var taskArguments: TaskArguments = null
  private var task: Task = null

  @invocable
  def construct(arguments: VariantArray): Unit = taskArguments = TaskArguments(arguments)

  @invocable
  def begin(objectAnys: VariantArray, objectNamesAnys: VariantArray): Boolean = {
    val namedInvocables = toNamedObjectMap(names = objectNamesAnys, anys = objectAnys)
    task = taskArguments.module match {
      case module: ShellModule ⇒
        new ShellProcessTask(
          jobName = taskArguments.jobName,
          module,
          namedInvocables,
          taskArguments.monitors,
          hasOrder = taskArguments.hasOrder,
          stdFileMap = taskStartArguments.stdFileMap,
          environment = taskStartArguments.environment.toImmutableSeq ++ taskArguments.environment)
      case module: JavaModule ⇒
        new JavaProcessTask(
          jobName = taskArguments.jobName,
          module,
          namedInvocables,
          taskArguments.monitors,
          stdFileMap = taskStartArguments.stdFileMap filter { _ ⇒ taskStartArguments.logStdoutAndStderr })
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

  def sendProcessSignal(signal: ProcessSignal) = {
    Logger(getClass).trace(s"sendProcessSignal $signal")
    task match {
      case o: HasSendProcessSignal ⇒ o.sendProcessSignal(signal)
    }
  }

  override def toString = List(
      s"${getClass.getSimpleName}",
      Option(taskArguments) map { t ⇒ s"(${t.jobName}:${t.taskId})" } getOrElse ""
    ).mkString("")
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
