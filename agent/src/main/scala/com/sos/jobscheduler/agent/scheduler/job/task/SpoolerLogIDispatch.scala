package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.agent.scheduler.job.task.SpoolerLogIDispatch._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.minicom.idispatch.annotation.invocable
import com.sos.jobscheduler.minicom.idispatch.{AnnotatedInvocable, InvocableIDispatch}
import com.sos.jobscheduler.minicom.remoting.proxy.{HasProxyMeta, ProxyMeta}
import com.sos.jobscheduler.taskserver.spoolerapi.ProxySpoolerLog

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
