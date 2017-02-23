package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits._
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.idispatch.{AnnotatedInvocable, DISPID, OverridingInvocableIDispatch}
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.SpecializedProxyIDispatch.forEachProperty
import com.sos.scheduler.engine.minicom.remoting.proxy.{ProxyIDispatchFactory, ProxyRemoting, SpecializedProxyIDispatch}
import com.sos.scheduler.engine.minicom.types.CLSID
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
final class ProxySpoolerLog private(protected val remoting: ProxyRemoting, val id: ProxyId, val name: String, initialMinimumLevel: SchedulerLogLevel)
extends SpoolerLog with SpecializedProxyIDispatch with AnnotatedInvocable with OverridingInvocableIDispatch {

  import com.sos.scheduler.engine.taskserver.spoolerapi.ProxySpoolerLog._

  private var _minimumLevel = initialMinimumLevel

  @invocable
  def level: Int = _minimumLevel.cppNumber

  @invocable
  def level_=(number: Int): Unit = {
    _minimumLevel = SchedulerLogLevel.ofCpp(number)
    this.invokePut(LevelDispid, number)
  }

  @invocable
  override def debug(message: String): Unit = debug1(message)

  @invocable
  def debug1(message: String) = log(SchedulerLogLevel.debug1, message)

  @invocable
  def debug2(message: String) = log(SchedulerLogLevel.debug2, message)

  @invocable
  def debug3(message: String) = log(SchedulerLogLevel.debug3, message)

  @invocable
  def debug4(message: String) = log(SchedulerLogLevel.debug4, message)

  @invocable
  def debug5(message: String) = log(SchedulerLogLevel.debug5, message)

  @invocable
  def debug6(message: String) = log(SchedulerLogLevel.debug6, message)

  @invocable
  def debug7(message: String) = log(SchedulerLogLevel.debug7, message)

  @invocable
  def debug8(message: String) = log(SchedulerLogLevel.debug8, message)

  @invocable
  def debug9(message: String) = log(SchedulerLogLevel.debug9, message)

  @invocable
  override def info(message: String) = log(SchedulerLogLevel.info, message)

  @invocable
  override def warn(message: String) = log(SchedulerLogLevel.warning, message)

  @invocable
  override def error(message: String) = log(SchedulerLogLevel.error, message)

  @invocable
  def log(level: Int, message: String): Unit = log(SchedulerLogLevel.ofCpp(level), message)

  def log(level: SchedulerLogLevel, message: String) = {
    if (_minimumLevel contains level) {
      this.invokeMethod(LogDispid, Vector(level.cppNumber, message))
    }
  }
}

object ProxySpoolerLog extends ProxyIDispatchFactory {
  val clsid = CLSID(UUID fromString "feee47a6-6c1b-11d8-8103-000476ee8afb")

  private[spoolerapi] val LogDispid = DISPID(14)
  private[spoolerapi] val LevelDispid = DISPID(19)

  def apply(remoting: ProxyRemoting, id: ProxyId, name: String, properties: Iterable[(String, Any)]) = {
    var level = SchedulerLogLevel.Min
    forEachProperty(properties, "sos.spooler.Log") {
      case ("level", v) ⇒ level = SchedulerLogLevel.ofCpp(v.asInstanceOf[Int])
    }
    new ProxySpoolerLog(remoting, id, name, initialMinimumLevel = level)
  }
}
