package com.sos.jobscheduler.master.gui.browser.components.state

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.data.event.{EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderAdded, OrderCanceled, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.system.Stdout
import com.sos.jobscheduler.data.workflow.WorkflowId
import com.sos.jobscheduler.data.workflow.position.WorkflowPosition
import com.sos.jobscheduler.master.gui.browser.common.Utils._
import com.sos.jobscheduler.master.gui.browser.components.state.OrdersState.{Content, OrderEntry}
import org.scalajs.dom.window
import scala.collection.immutable.{Seq, VectorBuilder}
import scala.collection.mutable
import scala.scalajs.js
import scala.util.control.NonFatal

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
        workflowToOrderIds = updatedIdToEntry.values groupBy (_.order.workflowId) mapValuesStrict (_.map(_.order.id).toVector.sorted),
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
    workflowToOrderIds: WorkflowToOrderIds,
    eventId: EventId,
    eventCount: Int,
    markedOrders: Map[Mark, OrderId] = Map.empty)
  extends Content
  {
    private val positionCache = mutable.Map[WorkflowId, mutable.Map[WorkflowPosition, Vector[OrderId]]]()

    def orderCountByWorkflow(workflowId: WorkflowId) =
      Some(workflowToOrderIds(workflowId).size)

    def workflowPositionToOrderIdSeq(workflowPosition: WorkflowPosition): Vector[OrderId] =
      positionCache.getOrElseUpdate(workflowPosition.workflowId, {
        val m = mutable.Map.empty[WorkflowPosition, VectorBuilder[OrderId]]
        for (orderId ← workflowToOrderIds.getOrElse(workflowPosition.workflowId, Nil)) {
          m.getOrElseUpdate(idToEntry(orderId).order.workflowPosition, new VectorBuilder[OrderId]) += orderId
        }
        m map { case (k, v) ⇒ k → v.result }
      }).getOrElseUpdate(workflowPosition, Vector.empty)

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
      val deleted = mutable.Set[OrderId]()
      val updated = mutable.Map[OrderId, OrderEntry]()
      val added = mutable.Map[WorkflowId, OrderedSet[OrderId]]()
      var evtCount = 0
      val nowMillis = Timestamp.currentTimeMillis
      stampedEvents foreach { stamped ⇒
        try stamped match {
          case Stamped(_, _, KeyedEvent(orderId, event: OrderAdded)) ⇒
            deleted -= orderId
            updated += orderId → OrderEntry(Order.fromOrderAdded(orderId, event), updatedAt = nowMillis)
            added.getOrElseUpdate(event.workflowId, new OrderedSet) += orderId
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
                      order = entry.order.update(event).orThrow,  // 🔥 ProblemException
                      lastOutputOfCurrentJob = lastOutput,
                      updatedAt = nowMillis)

                  case OrderStdWritten(t, chunk) ⇒
                    val output = entry.output ++ splitLines(chunk, if (t == Stdout) "›"/*>1*/ else "»"/*>2*/)
                    entry.copy(
                      output = output,
                      lastOutputOfCurrentJob = output.lastOption,
                      updatedAt = nowMillis)
                })
                event match {
                  case event: OrderForked ⇒
                    val childOrders = entry.order.newForkedOrders(event)
                    val childOrderIds = childOrders map (_.id)
                    deleted --= childOrderIds
                    updated ++= childOrders.map(o ⇒ o.id → OrderEntry(o, mark = entry.mark.map(_.copy(isRelative = true)), updatedAt = nowMillis))
                    added.getOrElseUpdate(entry.order.workflowId, new OrderedSet) ++= childOrderIds

                  case _: OrderJoined ⇒
                    for (order ← entry.order.ifState[Order.Forked]) {
                      val childOrderIds = order.state.childOrderIds
                      deleted ++= childOrderIds
                      updated --= childOrderIds
                      val w = entry.order.workflowId
                      if (added isDefinedAt w) {
                        added(w) --= childOrderIds
                      }
                    }

                  case OrderFinished | OrderCanceled ⇒
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
        } catch { case NonFatal(t) ⇒
          throw new RuntimeException(s"Error when handling $stamped: ${t.toStringWithCauses}", t)
        }
      }
      val updatedWorkflowPositions: Iterator[WorkflowPosition] = for {
        order ← updated.valuesIterator map (_.order)
        previousWorkflowPositions = idToEntry.get(order.id) map (_.order.workflowPosition) if !previousWorkflowPositions.contains(order.workflowPosition)
        pos ← Array(order.workflowPosition) ++ previousWorkflowPositions
      } yield pos
      val updatedIdToEntry = idToEntry -- deleted ++ updated
      copy(
        idToEntry = updatedIdToEntry,
        workflowToOrderIds = concatOrderIds(deleteOrderIds(workflowToOrderIds, deleted.toSet), added),
        eventId = stampedEvents.last.eventId,
        eventCount = eventCount + evtCount)
      .restoreCache(
        from = this,
        dirty = deleted.flatMap(o ⇒ idToEntry.get(o)).map(_.order.workflowPosition) ++ updatedWorkflowPositions)
    }

    def markOrder(orderId: OrderId, mark: Mark): FetchedContent =
      idToEntry.get(orderId) match {
        case Some(orderEntry) if !orderEntry.mark.contains(mark) ⇒
          val meAndRelatives = thisAndRelatives(orderId)
          copy(
            idToEntry = idToEntry ++
              meAndRelatives.flatMap(idToEntry.get)
                .map(o ⇒ o.id → o.copy(mark = Some(mark.copy(isRelative = o.id != orderId)))) ++
              (markedOrders.get(mark).toVector.flatMap(thisAndRelatives).toSet -- meAndRelatives)
                .flatMap(idToEntry.get)
                .map(o ⇒ o.id → o.copy(mark = None)),
            markedOrders = markedOrders.filterNot(o ⇒ o._1 == mark || o == (Mark.Volatile → orderId)) + (mark → orderId))
        case _ ⇒
          copy(markedOrders = markedOrders - mark)
      }

    def unmarkOrder(orderId: OrderId, mark: Mark): FetchedContent =
      idToEntry.get(orderId) match {
        case Some(orderEntry) if orderEntry.mark contains mark ⇒
          copy(
            idToEntry = idToEntry ++
              thisAndRelatives(orderId)
                .flatMap(idToEntry.get)
                .map(relative ⇒ relative.id → relative.copy(mark = None)),
            markedOrders = markedOrders filterNot (_ == (mark → orderId)))
        case _ ⇒ this
      }

    private def thisAndRelatives(orderId: OrderId): Set[OrderId] = {
      val known = mutable.Set[OrderId]()
      def loop(orderId: OrderId): Unit =
        for (orderEntry ← idToEntry.get(orderId)) {
          known += orderId
          val order = orderEntry.order
          val children = order.ifState[Order.Forked].toVector.flatMap(_.state.children.map(_.orderId))
          val added = (children ++ order.parent) filterNot known
          known ++= added
          added foreach loop
        }
      loop(orderId)
      known.toSet
    }

    //private def relatives(orderId: OrderId): Seq[OrderId] =
    //  descendants(orderId) ++ ancestors(orderId)
    //
    //private def descendants(orderId: OrderId): Seq[OrderId] =
    //  idToEntry.get(orderId).toVector flatMap { orderEntry ⇒
    //    val children = orderEntry.order.ifState[Order.Forked].toVector.flatMap(_.state.children.map(_.orderId))
    //    children ++ children.flatMap(descendants)
    //  }
    //
    //private def ancestors(orderId: OrderId): Seq[OrderId] =
    //  idToEntry.get(orderId).toVector flatMap (orderEntry ⇒
    //    orderEntry.order.parent ++: orderEntry.order.parent.toList.flatMap(ancestors))

    def isMarked(orderId: OrderId, mark: Mark) =
      idToEntry.get(orderId).exists(_.mark contains mark)
  }

  private def concatOrderIds(
    wToO: WorkflowToOrderIds,
    added: mutable.Map[WorkflowId, OrderedSet[OrderId]])
  : WorkflowToOrderIds =
      wToO ++
        (for ((workflowId, orderIds) ← added) yield
          workflowId →
            (wToO.get(workflowId) match {
              case Some(a) ⇒
                if (orderIds.array.isEmpty)
                  a
                else {
                  val isAdded = orderIds.array.toSet
                  if (a exists isAdded) a.filterNot(isAdded) ++ orderIds.array
                  else a ++ orderIds.array
                }
              case None ⇒
                orderIds.array.toVector
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
    mark: Option[Mark] = None,
    updatedAt: Long)
  {
    def id = order.id

    override def toString = s"OrderEntry($order)"
  }

  private class OrderedSet[A] {
    var array = new js.Array[A]

    def +=(a: A): Unit =
      if (!array.contains(a)) {
        array.push(a)
      }

    def -=(a: A): Unit =
      array = array.filterNot(_ == a)  // Optimierbar, wenn a letztes Element ist

    def ++=(as: Iterable[A]): Unit =
      if (as.nonEmpty) {
        val duplicates = array filter as.toSet
        if (duplicates.isEmpty)
          array.appendAll(as)
        else
          array = array.filterNot(duplicates.toSet) ++ as
      }

    def --=(as: Iterable[A]): Unit =  // Optimierbar, wenn alle as am Ende stehen
      array = array.filterNot(as.toSet)
  }

  final case class Mark(isVolatile: Boolean, isRelative: Boolean = false)
  object Mark {
    val Permanent = Mark(isVolatile = false)
    val Volatile = Mark(isVolatile = true)
  }
}


