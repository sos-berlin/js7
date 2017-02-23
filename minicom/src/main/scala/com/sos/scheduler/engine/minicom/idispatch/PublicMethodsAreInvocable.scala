package com.sos.scheduler.engine.minicom.idispatch

import java.lang.reflect.Method
import scala.collection.immutable.Seq

/**
  * All public methods are callable via [[OverridingInvocableIDispatch]]
  *
  * @author Joacim Zschimmer
  */
trait PublicMethodsAreInvocable extends Invocable {

  final def invocableMethods: Seq[(Method, Option[DISPID])] =
    getClass.getMethods.toVector map { _ → None }
}
