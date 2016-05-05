package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.minicom.types.IUnknown
import java.lang.reflect.Method

/**
 * Public methods annotated with @[[com.sos.scheduler.engine.minicom.idispatch.annotation.invocable]] are callable via [[OverridingInvocableIDispatch]].
 * Nearly a [[IDispatch]], but without need to implement the `IDispatch`s methods.
 *
 * @author Joacim Zschimmer
 */
trait Invocable extends IUnknown {
  def invocableMethods: Seq[Method]
}

object Invocable {
  trait Empty extends Invocable {
    final def invocableMethods = Nil
  }
}
