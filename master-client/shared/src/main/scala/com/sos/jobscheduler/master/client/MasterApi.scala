package com.sos.jobscheduler.master.client
import com.sos.jobscheduler.data.event.{EventId, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrdersOverview}
import com.sos.jobscheduler.data.workflow.WorkflowScript
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
trait MasterApi {

  def executeCommand(command: MasterCommand): Future[command.MyResponse]

  def overview: Future[MasterOverview]

  def ordersOverview: Future[OrdersOverview]

  def orders: Future[Stamped[Seq[Order[Order.State]]]]

  def orderEvents(after: EventId, timeout: Duration): Future[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]

  def workflowScripts: Future[Stamped[Seq[WorkflowScript.Named]]]
}
