package com.sos.jobscheduler.master.gui.components.state

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.event.{EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderForked, OrderJoined, OrderProcessed, OrderStdWritten, OrderTransitionedEvent}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey, WorkflowPath}
import com.sos.jobscheduler.master.gui.common.Utils._
import com.sos.jobscheduler.master.gui.components.state.OrdersState._
import org.scalajs.dom.window
import scala.collection.immutable.{Seq, VectorBuilder}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final case class OrdersState(
  content: Content,
  step: Int,
  selectedOrder: Option[OrderId] = None,
  error: Option[String] = None)
{
  def updateOrders(stamped: Stamped[Seq[Order[Order.State]]]): OrdersState = {
    val orders = stamped.value
    val updatedIdToEntry = orders.map(v ⇒ v.id → OrderEntry(v, updatedAt = 0)).toMap
    copy(
      content = OrdersState.FetchedContent(
        idToEntry = updatedIdToEntry,
        workflowToOrderSeq = updatedIdToEntry.values groupBy (_.order.workflowPath) mapValues (_.map(_.order.id).toVector.sorted),
        eventId = stamped.eventId, eventCount = 0),
      error = None,
      step = step + 1)
  }
}

object OrdersState {
  val Empty = OrdersState(Initial, step = 0, error = None)

  private type WorkflowToOrderIds = Map[WorkflowPath, Vector[OrderId]]
  sealed trait Content

  object Initial extends Content
  object FetchingContent extends Content

  final case class FetchedContent(
    idToEntry: Map[OrderId, OrderEntry],
    workflowToOrderSeq: WorkflowToOrderIds,
    eventId: EventId,
    eventCount: Int)
  extends Content
  {
    private val nodeKeyCache = mutable.Map[WorkflowPath, mutable.Map[NodeId, Vector[OrderId]]]()

    def nodeKeyToOrderIdSeq(nodeKey: NodeKey): Vector[OrderId] =
      nodeKeyCache.getOrElseUpdate(nodeKey.workflowPath, {
        val m = mutable.Map.empty[NodeId, VectorBuilder[OrderId]]
        for (orderId ← workflowToOrderSeq.getOrElse(nodeKey.workflowPath, Vector.empty)) {
          m.getOrElseUpdate(idToEntry(orderId).order.nodeId, new VectorBuilder[OrderId]) += orderId
        }
        m map { case (k, v) ⇒ k → v.result }
      }).getOrElseUpdate(nodeKey.nodeId, Vector.empty)

    private def restoreCache(from: FetchedContent, dirty: collection.Set[NodeKey]): this.type = {
      for ((workflowPath, nodeToOrderIds) ← from.nodeKeyCache -- dirty.map(_.workflowPath)) {
        val m = mutable.Map.empty[NodeId, Vector[OrderId]]
        for ((nodeId, orderIds) ← nodeToOrderIds -- dirty.map(_.nodeId)) {
          m(nodeId) = orderIds
        }
        nodeKeyCache(workflowPath)  = m
      }
      this
    }

    def handleEvents(stampedEvents: Seq[Stamped[KeyedEvent[OrderEvent]]]): FetchedContent = {
      val updated = mutable.Map[OrderId, OrderEntry]()
      val added = mutable.Map[WorkflowPath, mutable.Buffer[OrderId]]()
      val deleted = mutable.Set[OrderId]()
      var evtCount = 0
      val nowMillis = Timestamp.epochMilli
      stampedEvents foreach {
        case Stamped(_, KeyedEvent(orderId, event: OrderAdded)) ⇒
          updated += orderId → OrderEntry(Order.fromOrderAdded(orderId, event), updatedAt = nowMillis)
          added.getOrElseUpdate(event.nodeKey.workflowPath, mutable.Buffer()) += orderId
          deleted -= orderId
          evtCount += 1

        case Stamped(eId, KeyedEvent(orderId, event: OrderEvent)) ⇒
          updated.get(orderId) orElse idToEntry.get(orderId) match {
            case None ⇒ window.console.error("Unknown OrderId: " + eventToLog(eId, orderId, event))
            case Some(entry) ⇒
              updated += orderId → (event match {
                case event: OrderCoreEvent ⇒
                  val lastOutput = event match {
                    case _: OrderTransitionedEvent ⇒ None
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
                case OrderForked(children) ⇒
                  for (child ← children) {
                    val childOrder = entry.order.newChild(child)
                    updated += childOrder.id → OrderEntry(childOrder, updatedAt = nowMillis)
                    deleted -= childOrder.id
                    added.getOrElseUpdate(entry.order.workflowPath, mutable.Buffer()) += childOrder.id
                  }

                case _: OrderJoined ⇒
                  for (order ← entry.order.ifState[Order.Forked]) {
                    deleted ++= order.state.childOrderIds
                    updated --= order.state.childOrderIds
                    val w = entry.order.workflowPath
                    for (a ← added.get(w)) {
                      added(w) = a filterNot order.state.childOrderIds.toSet
                    }
                  }

                case _ ⇒
              }
              evtCount += 1
          }

        case Stamped(eId, KeyedEvent(orderId, event)) ⇒
          window.console.warn("Ignored: " + eventToLog(eId, orderId, event))

        case _ ⇒
      }
      val updatedNodeKeys: Iterator[NodeKey] = for {
        order ← updated.valuesIterator map (_.order)
        previousNodeKey = idToEntry.get(order.id) map (_.order.nodeKey) if !previousNodeKey.contains(order.nodeKey)
        nodeKey ← Array(order.nodeKey) ++ previousNodeKey
      } yield nodeKey
      val updatedIdToEntry = idToEntry -- deleted ++ updated
      copy(
        idToEntry = updatedIdToEntry,
        workflowToOrderSeq = concatOrderIds(deleteOrderIds(workflowToOrderSeq, deleted.toSet), added),
        eventId = stampedEvents.last.eventId,
        eventCount = eventCount + evtCount)
      .restoreCache(
        from = this,
        dirty = deleted.flatMap(o ⇒ idToEntry.get(o)).map(_.order.nodeKey) ++ updatedNodeKeys)
    }
  }

  private def concatOrderIds(
    wToO: WorkflowToOrderIds,
    added: mutable.Map[WorkflowPath, mutable.Buffer[OrderId]])
  : WorkflowToOrderIds =
      wToO ++
        (for ((workflowPath, orderIds) ← added) yield
          workflowPath →
            (wToO.get(workflowPath) match {
              case Some(a) ⇒ a ++ orderIds
              case None ⇒ orderIds.toVector
            })
        ).toMap

  private def deleteOrderIds(wToO: WorkflowToOrderIds, deleted: Set[OrderId]): WorkflowToOrderIds =
    if (deleted.isEmpty)
      wToO
    else
      wToO.mapValues {
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


