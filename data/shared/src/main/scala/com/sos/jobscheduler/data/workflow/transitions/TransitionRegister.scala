package com.sos.jobscheduler.data.workflow.transitions

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.workflow.transition.{ForwardTransition, SuccessFailureTransition, TransitionType}

/**
  * @author Joacim Zschimmer
  */
object TransitionRegister {

  implicit val JsonCodec = TypedJsonCodec[TransitionType](
    Subtype(SuccessFailureTransition),
    Subtype(ForwardTransition),
    Subtype(ForkTransition),
    Subtype(JoinTransition)
  )
}
