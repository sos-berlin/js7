package com.sos.scheduler.engine.agent.orderprocessing.job.task

import com.sos.scheduler.engine.agent.orderprocessing.job.task.SpoolerLogIDispatch._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.idispatch.{AnnotatedInvocable, InvocableIDispatch}
import com.sos.scheduler.engine.minicom.remoting.proxy.{HasProxyMeta, ProxyMeta}
import com.sos.scheduler.engine.taskserver.spoolerapi.ProxySpoolerLog

/**
  * @author Joacim Zschimmer
  */
final class SpoolerLogIDispatch extends InvocableIDispatch with HasProxyMeta with AnnotatedInvocable {

  def proxyMeta = ProxyMeta(
    ProxySpoolerLog.clsid,
    Map("level" → 0))

  @invocable(dispId = 14)
  def log(level: Int, message: String): Unit = {
    logger.info(s"level=$level $message")
  }
}

object SpoolerLogIDispatch {
  private val logger = Logger(getClass)
}
