package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.types.IUnknown
import java.lang.reflect.Method

/**
 * Public methods annotated with @[[com.sos.scheduler.engine.minicom.idispatch.annotation.invocable]] are callable via [[InvocableIDispatch]].
 * Nearly a [[IDispatch]], but without need to implement the `IDispatch`s methods.
 *
 * @author Joacim Zschimmer
 */
trait Invocable extends IUnknown {
  def invocableMethods: Seq[Method] = getClass.getMethods filter { _.getAnnotation(classOf[invocable]) != null }
}

object Invocable {
  object Empty extends Invocable
}
