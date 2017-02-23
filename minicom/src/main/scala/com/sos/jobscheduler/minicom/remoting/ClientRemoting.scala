package com.sos.jobscheduler.minicom.remoting

import com.sos.jobscheduler.minicom.idispatch.IUnknownFactory
import com.sos.jobscheduler.minicom.remoting.dialog.ClientDialogConnection
import com.sos.jobscheduler.minicom.remoting.proxy.ProxyIDispatchFactory
import com.sos.jobscheduler.minicom.types.IUnknown
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
