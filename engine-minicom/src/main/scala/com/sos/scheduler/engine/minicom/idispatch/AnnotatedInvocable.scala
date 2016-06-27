package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import java.lang.reflect.Method

/**
  * Methods annotated with @[[invocable]] are callable via [[OverridingInvocableIDispatch]].
  *
  * @author Joacim Zschimmer
  */
trait AnnotatedInvocable extends Invocable {
  final def invocableMethods: Seq[Method] = getClass.getMethods filter { _.getAnnotation(classOf[invocable]) != null }
}
