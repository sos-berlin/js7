package com.sos.scheduler.engine.minicom.idispatch

/**
  * An IDispatch using only methods of Invocable.
  * *
  * @author Joacim Zschimmer
  */
trait InvocableIDispatch extends IDispatch.Empty with OverridingInvocableIDispatch
