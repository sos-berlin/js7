package com.sos.scheduler.engine.minicom.remoting.invoker

import com.sos.scheduler.engine.minicom.idispatch.IDispatch

/**
 * @author Joacim Zschimmer
 */
trait IDispatchInvoker extends sos.spooler.Invoker {
  def iDispatch: IDispatch
}
