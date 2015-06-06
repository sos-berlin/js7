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
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatchFactory, SpecializedProxyIDispatch}
import com.sos.scheduler.engine.minicom.types.CLSID
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.{Stderr, Stdout, StdoutStderrType}
import java.nio.charset.StandardCharsets.ISO_8859_1
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
final class ProxySpoolerTask private(taskStartArguments: TaskStartArguments, protected val remoting: ClientRemoting, val id: ProxyId, val name: String)
extends SpoolerTask with SpecializedProxyIDispatch {

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
}

object ProxySpoolerTask extends ProxyIDispatchFactory {
  val clsid = CLSID(UUID fromString "feee47aa-6c1b-11d8-8103-000476ee8afb")
  private val logger = Logger(getClass)

  def apply(injector: Injector, remoting: ClientRemoting, id: ProxyId, name: String, properties: Iterable[(String, Any)]) = {
    if (properties.nonEmpty) logger.warn(s"IGNORED: $properties")  // TODO Weitere lokale Properties
    new ProxySpoolerTask(injector.instance[TaskStartArguments], remoting, id, name)
  }
}
