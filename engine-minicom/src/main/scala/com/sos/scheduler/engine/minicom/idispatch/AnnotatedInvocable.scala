package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import java.lang.reflect.Method
import scala.collection.immutable.Seq

/**
  * Methods annotated with @[[invocable]] are callable via [[OverridingInvocableIDispatch]].
  *
  * @author Joacim Zschimmer
  */
trait AnnotatedInvocable extends Invocable {

  final def invocableMethods: Seq[(Method, Option[DISPID])] =
    getClass.getMethods.toVector map { m ⇒
      m → m.getAnnotation(classOf[invocable])
    } collect {
      case (m, a) if a != null ⇒
        m → (a.dispId != invocable.UnusedDispId option DISPID(a.dispId))
    }
}
