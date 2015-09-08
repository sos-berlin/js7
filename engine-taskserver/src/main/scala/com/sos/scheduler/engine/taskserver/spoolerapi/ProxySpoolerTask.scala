package com.sos.scheduler.engine.taskserver.spoolerapi

import com.google.inject.Injector
import com.sos.scheduler.engine.common.guice.GuiceImplicits.RichInjector
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.data.message.MessageCode
import com.sos.scheduler.engine.minicom.idispatch.DISPID
import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits._
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.SpecializedProxyIDispatch._
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatchFactory, SpecializedProxyIDispatch}
import com.sos.scheduler.engine.minicom.types.CLSID
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.{Stderr, Stdout, StdoutStderrType}
import java.nio.charset.StandardCharsets.ISO_8859_1
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
final class ProxySpoolerTask private(taskStartArguments: TaskStartArguments, protected val remoting: ClientRemoting, val id: ProxyId, val name: String)
extends SpoolerTask with SpecializedProxyIDispatch {

  import ProxySpoolerTask._

  def setErrorCodeAndText(code: MessageCode, text: String): Unit =
    this.invokeMethod(DISPID(26), List(code.string, text))

  def paramsXml = this.invokeGet(DISPID(35)).asInstanceOf[String]

  def paramsXml_=(o: String) = this.invokePut(DISPID(35), o)

  def orderParamsXml = this.invokeGet(DISPID(36)).asInstanceOf[String]

  def orderParamsXml_=(o: String) = this.invokePut(DISPID(36), o)

  @invocable
  def stdout_path: String = filePath(Stdout)

  @invocable
  def stderr_path: String = filePath(Stderr)

  @invocable
  def stdout_text: String = fileText(Stdout)

  @invocable
  def stderr_text: String = fileText(Stderr)

  private def fileText(s: StdoutStderrType) = taskStartArguments.stdFileMap.get(s) map { _.contentString(ISO_8859_1) } getOrElse ""

  private def filePath(s: StdoutStderrType) = taskStartArguments.stdFileMap.get(s) map { _.toString } getOrElse ""

  @invocable
  def create_subprocess(program_and_parameters: Option[AnyRef]) = throw new UnsupportedApiException("sos.spooler.Task.create_subprocess")

  @invocable
  def priority_class: String = throw new UnsupportedApiException("sos.spooler.Task.priority_class")

  @invocable
  def priority_class_=(o: String): Unit = logger.warn(s"Ignoring sos.spooler.Task.priority_class='$o'")

  @invocable
  def priority: Int = throw new UnsupportedApiException("sos.spooler.Task.priority")

  @invocable
  def priority_=(o: Int): Unit = logger.warn(s"Ignoring sos.spooler.Task.priority=$o")
}

object ProxySpoolerTask extends ProxyIDispatchFactory {
  val clsid = CLSID(UUID fromString "feee47aa-6c1b-11d8-8103-000476ee8afb")
  private val logger = Logger(getClass)

  def apply(injector: Injector, remoting: ClientRemoting, id: ProxyId, name: String, properties: Iterable[(String, Any)]) = {
    forEachProperty(properties, "sos.spooler.Task") {
      case ("subprocess_own_process_group_default", v: Boolean) ⇒ if (v) logger.trace(s"Universal Agent does not support subprocess.own_process_group=true")
    }
    new ProxySpoolerTask(injector.instance[TaskStartArguments], remoting, id, name)
  }
}
