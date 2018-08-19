package com.sos.jobscheduler.master.gui.browser.components.state

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections._
import com.sos.jobscheduler.data.event.{EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderAdded, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{WorkflowId, WorkflowPosition}
import com.sos.jobscheduler.master.gui.browser.common.Utils._
import com.sos.jobscheduler.master.gui.browser.components.state.OrdersState.{Content, OrderEntry}
import org.scalajs.dom.window
import scala.collection.immutable.{Seq, VectorBuilder}
import scala.collection.mutable
import scala.scalajs.js

/**
  * @author Joacim Zschimmer
  */
final case class OrdersState(
  content: Content,
  step: Int,
  error: Option[String] = None)
{
  def orderCountByWorkflow(workflowId: WorkflowId): Option[Int] =
    content.orderCountByWorkflow(workflowId)

  def updateOrders(stamped: Stamped[Seq[Order[Order.State]]]): OrdersState = {
    val orders = stamped.value
    val updatedIdToEntry = orders.map(o ⇒ o.id → OrderEntry(o, updatedAt = 0)).toMap
    copy(
      content = OrdersState.FetchedContent(
        idToEntry = updatedIdToEntry,
        workflowToOrderSeq = updatedIdToEntry.values groupBy (_.order.workflowId) mapValuesStrict (_.map(_.order.id).toVector.sorted),
        eventId = stamped.eventId, eventCount = 0),
      error = None,
      step = step + 1)
  }
}

object OrdersState {
  val Empty = OrdersState(Starting, step = 0, error = None)

  private type WorkflowToOrderIds = Map[WorkflowId, Vector[OrderId]]
  sealed trait Content {
    def orderCountByWorkflow(workflowId: WorkflowId): Option[Int]
  }

  object Starting extends Content {
    def orderCountByWorkflow(workflowId: WorkflowId) = None
  }
  object FetchingContent extends Content {
    def orderCountByWorkflow(workflowId: WorkflowId) = None
  }

  final case class FetchedContent(
    idToEntry: Map[OrderId, OrderEntry],
    workflowToOrderSeq: WorkflowToOrderIds,
    eventId: EventId,
    eventCount: Int)
  extends Content
  {
    private val positionCache = mutable.Map[WorkflowId, mutable.Map[WorkflowPosition, Vector[OrderId]]]()

    def orderCountByWorkflow(workflowId: WorkflowId) =
      Some(workflowToOrderSeq(workflowId).size)

    def workflowPositionToOrderIdSeq(address: WorkflowPosition): Vector[OrderId] =
      positionCache.getOrElseUpdate(address.workflowId, {
        val m = mutable.Map.empty[WorkflowPosition, VectorBuilder[OrderId]]
        for (orderId ← workflowToOrderSeq.getOrElse(address.workflowId, Nil)) {
          m.getOrElseUpdate(idToEntry(orderId).order.workflowPosition, new VectorBuilder[OrderId]) += orderId
        }
        m map { case (k, v) ⇒ k → v.result }
      }).getOrElseUpdate(address, Vector.empty)

    private def restoreCache(from: FetchedContent, dirty: collection.Set[WorkflowPosition]): this.type = {
      for ((workflowPath, positionToOrderIds) ← from.positionCache -- dirty.map(_.workflowId)) {
        val m = mutable.Map.empty[WorkflowPosition, Vector[OrderId]]
        for ((position, orderIds) ← positionToOrderIds -- dirty) {
          m(position) = orderIds
        }
        positionCache(workflowPath) = m
      }
      this
    }

    def handleEvents(stampedEvents: Seq[Stamped[KeyedEvent[OrderEvent]]]): FetchedContent = {
      val updated = mutable.Map[OrderId, OrderEntry]()
      val added = mutable.Map[WorkflowId, js.Array[OrderId]]()
      val deleted = mutable.Set[OrderId]()
      var evtCount = 0
      val nowMillis = Timestamp.currentTimeMillis
      stampedEvents foreach {
        case Stamped(_, _, KeyedEvent(orderId, event: OrderAdded)) ⇒
          updated += orderId → OrderEntry(Order.fromOrderAdded(orderId, event), updatedAt = nowMillis)
          added.getOrElseUpdate(event.workflowId, new js.Array) += orderId
          deleted -= orderId
          evtCount += 1

        case Stamped(eId, _, KeyedEvent(orderId, event: OrderEvent)) ⇒
          updated.get(orderId) orElse idToEntry.get(orderId) match {
            case None ⇒ window.console.error("Unknown OrderId: " + eventToLog(eId, orderId, event))
            case Some(entry) ⇒
              updated += orderId → (event match {
                case event: OrderCoreEvent ⇒
                  val lastOutput = event match {
                    case _: OrderActorEvent ⇒ None
                    case _ ⇒ entry.lastOutputOfCurrentJob
                  }
                  entry.copy(
                    order = entry.order.update(event),
                    lastOutputOfCurrentJob = lastOutput,
                    updatedAt = nowMillis)

                case OrderStdWritten(t, chunk) ⇒
                  val output = entry.output ++ splitLines(chunk, s"$t: ")
                  entry.copy(
                    output = output,
                    lastOutputOfCurrentJob = output.lastOption,
                    updatedAt = nowMillis)
              })
              event match {
                case event: OrderForked ⇒
                  for (childOrder ← entry.order.newForkedOrders(event)) {
                    updated += childOrder.id → OrderEntry(childOrder, updatedAt = nowMillis)
                    deleted -= childOrder.id
                    added.getOrElseUpdate(entry.order.workflowId, new js.Array) += childOrder.id
                  }

                case _: OrderJoined ⇒
                  for (order ← entry.order.ifState[Order.Join]) {
                    deleted ++= order.state.joinOrderIds
                    updated --= order.state.joinOrderIds
                    val w = entry.order.workflowId
                    for (a ← added.get(w)) {
                      added(w) = a filterNot order.state.joinOrderIds.toSet
                    }
                  }

                case _: OrderFinished ⇒
                  deleted += orderId
                  updated -= orderId
                  for (orderIds ← added.get(entry.order.workflowId))
                    orderIds -= orderId

                case _ ⇒
              }
              evtCount += 1
          }

        case Stamped(eId, _, KeyedEvent(orderId, event)) ⇒
          window.console.warn("Ignored: " + eventToLog(eId, orderId, event))

        case _ ⇒
      }
      val updatedWorkflowPositions: Iterator[WorkflowPosition] = for {
        order ← updated.valuesIterator map (_.order)
        previousWorkflowPositions = idToEntry.get(order.id) map (_.order.workflowPosition) if !previousWorkflowPositions.contains(order.workflowPosition)
        pos ← Array(order.workflowPosition) ++ previousWorkflowPositions
      } yield pos
      val updatedIdToEntry = idToEntry -- deleted ++ updated
      copy(
        idToEntry = updatedIdToEntry,
        workflowToOrderSeq = concatOrderIds(deleteOrderIds(workflowToOrderSeq, deleted.toSet), added),
        eventId = stampedEvents.last.eventId,
        eventCount = eventCount + evtCount)
      .restoreCache(
        from = this,
        dirty = deleted.flatMap(o ⇒ idToEntry.get(o)).map(_.order.workflowPosition) ++ updatedWorkflowPositions)
    }
  }

  private def concatOrderIds(
    wToO: WorkflowToOrderIds,
    added: mutable.Map[WorkflowId, js.Array[OrderId]])
  : WorkflowToOrderIds =
      wToO ++
        (for ((workflowId, orderIds) ← added) yield
          workflowId →
            (wToO.get(workflowId) match {
              case Some(a) ⇒ a ++ orderIds
              case None ⇒ orderIds.toVector
            })
        ).toMap

  private def deleteOrderIds(wToO: WorkflowToOrderIds, deleted: Set[OrderId]): WorkflowToOrderIds =
    if (deleted.isEmpty)
      wToO
    else
      wToO.mapValuesStrict {
        case v if v exists deleted ⇒ v filterNot deleted
        case v ⇒ v
      }

  private def splitLines(chunk: String, linePrefix: String) =
    chunk.split("\n").map(line ⇒ s"$linePrefix$line")

  //private def lastLineOfChunk(chunk: String): String = {
  //  val c = chunk.trim
  //  val start = c.lastIndexOf("\n") match {
  //    case -1 ⇒ 0
  //    case i ⇒ i + 1
  //  }
  //  c.substring(start).truncateWithEllipsis(50, showLength = false)
  //}

  final case class OrderEntry(
    order: Order[Order.State],
    output: Vector[String] = Vector(),
    lastOutputOfCurrentJob: Option[String] = None,
    updatedAt: Long)
  {
    def id = order.id
  }
}


