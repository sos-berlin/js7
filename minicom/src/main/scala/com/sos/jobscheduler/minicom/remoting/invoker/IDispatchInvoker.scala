package com.sos.jobscheduler.minicom.remoting.invoker

import com.sos.jobscheduler.minicom.idispatch.IDispatch

/**
 * @author Joacim Zschimmer
 */
trait IDispatchInvoker extends sos.spooler.Invoker {
  def iDispatch: IDispatch
}
