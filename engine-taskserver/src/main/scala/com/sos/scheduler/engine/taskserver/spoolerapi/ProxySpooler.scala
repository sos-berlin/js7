package com.sos.scheduler.engine.taskserver.spoolerapi

import com.google.inject.Injector
import com.sos.scheduler.engine.common.guice.GuiceImplicits.RichInjector
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.SpecializedProxyIDispatch._
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatchFactory, SpecializedProxyIDispatch}
import com.sos.scheduler.engine.minicom.types.CLSID
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
final class ProxySpooler private(protected val remoting: ClientRemoting, val id: ProxyId, val name: String, taskStartArguments: TaskStartArguments)
extends SpecializedProxyIDispatch {

  @invocable
  def directory: String = (taskStartArguments.directory resolve ".").toString stripSuffix "."  // Should end with "/"

  @invocable
  def include_path: String = throw new UnsupportedApiException("sos.spooler.Spooler.include_path")

  @invocable
  def ini_path: String = throw new UnsupportedApiException("sos.spooler.Spooler.ini_path")

  @invocable
  def log_dir: String = throw new UnsupportedApiException("sos.spooler.Spooler.log_dir")
}

object ProxySpooler extends ProxyIDispatchFactory {
  val clsid = CLSID(UUID fromString "feee47b3-6c1b-11d8-8103-000476ee8afb")

  def apply(injector: Injector, remoting: ClientRemoting, id: ProxyId, name: String, properties: Iterable[(String, Any)]) = {
    requireNoProperties(properties, "sos.spooler.Spooler")
    new ProxySpooler(remoting, id, name, injector.instance[TaskStartArguments])
  }
}
