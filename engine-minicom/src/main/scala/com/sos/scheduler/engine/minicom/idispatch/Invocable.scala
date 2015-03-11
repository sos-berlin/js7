package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.idispatch.Invocable._
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.types.IUnknown
import java.lang.reflect.Method

/**
 * Public methods annotated with @[[invocable]] are callable via [[InvocableIDispatch]].
 * Nearly a [[IDispatch]], but without need to implement the `IDispatch`s methods.
 *
 * @author Joacim Zschimmer
 */
trait Invocable extends IUnknown {

  def invocableMethods: Seq[Method] = {
    val r = getClass.getMethods filter { _.getAnnotation(classOf[invocable]) != null }
    if (r.nonEmpty) logger.warn(s"$getClass contains no methods declared with @invocable")
    r
  }
}

object Invocable {
  private val logger = Logger(getClass)
}
