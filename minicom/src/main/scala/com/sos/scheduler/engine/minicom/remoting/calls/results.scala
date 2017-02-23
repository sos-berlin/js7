package com.sos.scheduler.engine.minicom.remoting.calls

import com.sos.scheduler.engine.minicom.idispatch.{DISPID, IDispatch}
import com.sos.scheduler.engine.minicom.types.IUnknown
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
private[remoting] trait Result

private[remoting] object EmptyResult extends Result


private[remoting] final case class CreateInstanceResult(iUnknown: IUnknown)
extends Result


private[remoting] case object ReleaseResult
extends Result


private[remoting] final case class QueryInterfaceResult(iDispatch: IDispatch)
extends Result


private[remoting] final case class GetIDsOfNamesResult(dispatchIds: immutable.Seq[DISPID])
extends Result


private[remoting] final case class InvokeResult(result: Any)
extends Result
