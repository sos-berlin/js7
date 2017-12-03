package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
object TransitionRegister {

  implicit val JsonCodec = TypedJsonCodec[TransitionType](
    Subtype(SuccessFailureTransition),
    Subtype(ForwardTransition)
  )
}
