package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.idispatch.{AnnotatedInvocable, OverridingInvocableIDispatch}
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.SpecializedProxyIDispatch._
import com.sos.scheduler.engine.minicom.remoting.proxy.{ProxyIDispatchFactory, ProxyRemoting, SpecializedProxyIDispatch}
import com.sos.scheduler.engine.minicom.types.CLSID
import com.sos.scheduler.engine.taskserver.data.TaskServerArguments
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
final class ProxySpooler private(protected val remoting: ProxyRemoting, val id: ProxyId, val name: String, taskServerArguments: TaskServerArguments)
extends SpecializedProxyIDispatch with AnnotatedInvocable with OverridingInvocableIDispatch {

  @invocable
  def directory: String = (taskServerArguments.workingDirectory resolve ".").toString stripSuffix "."  // Should end with "/"

  @invocable
  def include_path: String = throw new UnsupportedApiException("sos.spooler.Spooler.include_path")

  @invocable
  def ini_path: String = throw new UnsupportedApiException("sos.spooler.Spooler.ini_path")

  @invocable
  def log_dir: String = throw new UnsupportedApiException("sos.spooler.Spooler.log_dir")

  @invocable
  def create_xslt_stylesheet(file: Option[String]) = throw new UnsupportedApiException("sos.spooler.Spooler.create_xslt_stylesheet")
}

object ProxySpooler {

  trait Factory extends ProxyIDispatchFactory {
    final val clsid = CLSID(UUID fromString "feee47b3-6c1b-11d8-8103-000476ee8afb")

    def taskServerArguments: TaskServerArguments

    final def apply(remoting: ProxyRemoting, id: ProxyId, name: String, properties: Iterable[(String, Any)]) = {
      requireNoProperties(properties, "sos.spooler.Spooler")
      new ProxySpooler(remoting, id, name, taskServerArguments)
    }
  }
}
