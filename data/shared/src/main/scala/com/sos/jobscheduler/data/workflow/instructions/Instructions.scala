package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils.singletonCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.OrderMoved
import com.sos.jobscheduler.data.workflow.{Instruction, OrderContext}

/**
  * @author Joacim Zschimmer
  */
object Instructions
{
  private[instructions] def ifProcessedThenOrderMoved(order: Order[Order.State], context: OrderContext) =
    order.ifState[Order.Processed].map(order ⇒
      order.id <-: OrderMoved(order.position.increment))

  private[workflow] implicit val jsonCodec: TypedJsonCodec[Instruction] = TypedJsonCodec[Instruction](
    Subtype[AwaitOrder],
    Subtype[Job],
    Subtype.named(singletonCodec(ExplicitEnd), "End"),
    Subtype(singletonCodec(ImplicitEnd)),  // Serialized for easier external use of Workflow
    Subtype(ForkJoin.jsonCodec),
    Subtype[Offer],
    Subtype[IfReturnCode],
    Subtype[IfNonZeroReturnCodeGoto],
    Subtype[Goto],
    Subtype(singletonCodec(Gap)))
}
