package com.sos.scheduler.engine.minicom.remoting

import com.google.inject.Injector
import com.sos.scheduler.engine.minicom.idispatch.IUnknownFactory
import com.sos.scheduler.engine.minicom.remoting.dialog.ClientDialogConnection
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyIDispatchFactory
import com.sos.scheduler.engine.minicom.types.IUnknown

/**
  * @author Joacim Zschimmer
  */
final class RemotingClient(
  protected val injector: Injector,
  protected val connection: ClientDialogConnection,
  protected val name: String,
  protected val proxyIDispatchFactories: Iterable[ProxyIDispatchFactory] = Nil,
  protected val invocableFactories: Iterable[IUnknownFactory] = Nil)
extends Remoting with ExecutingRemoting {

  protected def onReleased(iUnknown: IUnknown) = {}
}
