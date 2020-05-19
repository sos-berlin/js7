package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.data.MasterState
import com.sos.jobscheduler.proxy.javaapi.utils.VavrConversions._
import io.vavr.control.{Either => VEither}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.jdk.StreamConverters._

@javaApi
final class JMasterState(val underlying: MasterState)
extends JavaWrapper
{
  protected type Underlying = MasterState

  def eventId: Long =
    underlying.eventId

  def idToWorkflow(workflowId: JWorkflowId): VEither[Problem, JWorkflow] =
    underlying.repo.idTo[Workflow](workflowId.underlying)
      .map(JWorkflow.apply)
      .asVavr

  def orderIds: java.util.Set[OrderId] =
    underlying.idToOrder.keySet.asJava

  def idToOrder(orderId: OrderId): java.util.Optional[JOrder] =
      underlying.idToOrder.get(orderId)
        .map(JOrder.apply)
        .toJava

  lazy val eagerIdToOrder: java.util.Map[OrderId, JOrder] =
    underlying.idToOrder
      .view.values.map(JOrder.apply)
      .toKeyedMap(_.id)
      .asJava

  def ordersBy(predicate: Order[Order.State] => Boolean): java.util.stream.Stream[JOrder] =
    underlying.idToOrder
      .valuesIterator
      .filter(predicate)
      .map(JOrder.apply)
      .asJavaSeqStream
}
