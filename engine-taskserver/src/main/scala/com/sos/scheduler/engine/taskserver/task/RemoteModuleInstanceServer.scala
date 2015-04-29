package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.cast
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.idispatch.{Invocable, InvocableFactory}
import com.sos.scheduler.engine.minicom.types.{CLSID, IID, VariantArray}
import com.sos.scheduler.engine.taskserver.module.Module._
import com.sos.scheduler.engine.taskserver.module.java.JavaModule
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import com.sos.scheduler.engine.taskserver.module.{JavaModuleLanguage, NamedInvocables, ShellModuleLanguage}
import java.util.UUID
import javax.inject.Inject
import org.scalactic.Requirements._

/**
 * @author Joacim Zschimmer
 * @see Com_remote_module_instance_server, spooler_module_remote_server.cxx
 */
final class RemoteModuleInstanceServer @Inject private(taskStartArguments: TaskStartArguments) extends Invocable with HasCloser {
  import com.sos.scheduler.engine.taskserver.task.RemoteModuleInstanceServer._

  private var taskArguments: TaskArguments = null
  private var task: Task = null
  private var openCalled = false
  private var exitCalled = false

  @invocable
  def construct(arguments: VariantArray): Unit = taskArguments = TaskArguments(arguments)

  @invocable
  def begin(objectAnys: VariantArray, objectNamesAnys: VariantArray): Boolean = {
    task = taskArguments.moduleLanguage match {
      case ShellModuleLanguage ⇒
        new ShellProcessTask(
          ShellModule(taskArguments.script),
          toNamedObjectMap(names = objectNamesAnys, anys = objectAnys),
          taskArguments.monitors,
          jobName = taskArguments.jobName,
          hasOrder = taskArguments.hasOrder,
          environment = taskStartArguments.environment ++ taskArguments.environment)
        .closeWithCloser
      case JavaModuleLanguage ⇒
        def newClassInstance() = Class.forName(taskArguments.javaClassName).newInstance()
        new JavaProcessTask(
          JavaModule(newClassInstance),
          toNamedObjectMap(names = objectNamesAnys, anys = objectAnys),
          taskArguments.monitors,
          jobName = taskArguments.jobName,
          hasOrder = taskArguments.hasOrder,
          environment = taskStartArguments.environment ++ taskArguments.environment)
    }
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
  def call(javaSignature: String): Any =
    if (((javaSignature == SpoolerOnSuccessSignature || javaSignature == SpoolerOnErrorSignature) && !openCalled) ||
      (javaSignature == SpoolerExitSignature && (task == null || exitCalled))) {
      // C++ spooler_task.cxx behaves differently under Windows and Unix.
      // Also, some (superfluous) calls are made after an error occurred.
      logger.debug(s"Call ignored: $javaSignature")
      ()
    } else {
      requireState(task != null)
      javaSignature match {
        case SpoolerOpenSignature ⇒ openCalled = true
        case SpoolerExitSignature ⇒ exitCalled = true
        case _ ⇒
      }
      task.callIfExists(javaSignature)
    }

  @invocable
  def waitForSubprocesses(): Unit = {}

  override def toString =
    List(
      s"${getClass.getSimpleName}",
      Option(taskArguments) map { t ⇒ s"(${t.jobName}:${t.taskId}})" })
      .mkString("")
}

object RemoteModuleInstanceServer extends InvocableFactory {
  val clsid = CLSID(UUID fromString "feee47a3-6c1b-11d8-8103-000476ee8afb")
  val iid   = IID  (UUID fromString "feee47a2-6c1b-11d8-8103-000476ee8afb")
  private val logger = Logger(getClass)

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
