package com.sos.scheduler.engine.minicom.remoting

import com.sos.scheduler.engine.minicom.idispatch.IUnknownFactory
import com.sos.scheduler.engine.minicom.remoting.dialog.ClientDialogConnection
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyIDispatchFactory
import com.sos.scheduler.engine.minicom.types.IUnknown
import scala.collection.immutable
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class ClientRemoting(
  protected val connection: ClientDialogConnection,
  protected val name: String,
  protected val iUnknownFactories: immutable.Iterable[IUnknownFactory] = Nil,
  protected val proxyIDispatchFactories: immutable.Iterable[ProxyIDispatchFactory] = Nil)
  (implicit protected val executionContext: ExecutionContext)
extends Remoting {

  protected def onReleased(iUnknown: IUnknown) = {}
}
