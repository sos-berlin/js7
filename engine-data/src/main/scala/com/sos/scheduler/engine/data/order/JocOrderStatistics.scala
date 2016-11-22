package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.order.JocOrderStatistics._
import com.sos.scheduler.engine.data.order.OrderProcessingState._
import com.sos.scheduler.engine.data.queries.QueryableOrder
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class JocOrderStatistics(
  total: Int,
  notPlanned: Int,
  planned: Int,
  due: Int,
  started: Int,
  inTask: Int,
  inTaskProcess: Int,
  occupiedByClusterMember: Int,
  setback: Int,
  waitingForResource: Int,
  suspended: Int,
  blacklisted: Int,
  permanent: Int,
  fileOrder: Int)
{
  assert(total < 0 ||  // Negative values only to simplify tests
         total == notPlanned + planned + due + started + suspended + blacklisted &&
         started == inTask + occupiedByClusterMember + setback + waitingForResource &&
         inTask >= inTaskProcess &&
         total >= permanent + fileOrder,
    s"$this does not sum up correctly")

  def +(o: JocOrderStatistics) = JocOrderStatistics(
    total = total + o.total,
    notPlanned = notPlanned + o.notPlanned,
    planned = planned + o.planned,
    due = due + o.due,
    started = started + o.started,
    inTask = inTask + o.inTask,
    inTaskProcess = inTaskProcess + o.inTaskProcess,
    occupiedByClusterMember = occupiedByClusterMember + o.occupiedByClusterMember,
    setback = setback + o.setback,
    waitingForResource = waitingForResource + o.waitingForResource,
    suspended = suspended + o.suspended,
    blacklisted = blacklisted + o.blacklisted,
    permanent = permanent + o.permanent,
    fileOrder = fileOrder + o.fileOrder)

  override def toString = List(
      format("total", total),
      format("notPlanned", notPlanned),
      format("planned", planned),
      format("due", due),
      format("started", started),
      format("inTask", inTask),
      format("inTaskProcess", inTaskProcess),
      format("occupiedByClusterMember", occupiedByClusterMember),
      format("setback", setback),
      format("waitingForResource", waitingForResource),
      format("suspended", suspended),
      format("blacklisted", blacklisted),
      format("permanent", permanent),
      format("fileOrder", fileOrder))
    .mkString("JocOrderStatistics(", " ", ")")
}

object JocOrderStatistics {
  val Zero = JocOrderStatistics(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  implicit val MyJsonFormat = jsonFormat14(apply)

  private def format(name: String, number: Int) = s"$name=$number"

  final class Mutable(
    var total: Int = 0,
    var notPlanned: Int = 0,
    var planned: Int = 0,
    var due: Int = 0,
    var started: Int = 0,
    var inTask: Int = 0,
    var inTaskProcess: Int = 0,
    var occupiedByClusterMember: Int = 0,
    var setback: Int = 0,
    var waitingForResource: Int = 0,
    var suspended: Int = 0,
    var blacklisted: Int = 0,
    var permanent: Int = 0,
    var fileOrder: Int = 0)
  {
    def +=(o: JocOrderStatistics) = {
      total       += o.total
      notPlanned  += o.notPlanned
      planned     += o.planned
      due         += o.due
      started     += o.started
      inTask      += o.inTask
      inTaskProcess   += o.inTaskProcess
      occupiedByClusterMember += o.occupiedByClusterMember
      setback     += o.setback
      waitingForResource += o.waitingForResource
      suspended   += o.suspended
      blacklisted += o.blacklisted
      permanent   += o.permanent
      fileOrder   += o.fileOrder
    }

    /**
      * Addition for JOC Cockpit - `suspended` counts differently.
      */
    def count(order: QueryableOrder): Unit = {
      total     += 1
      permanent += toInt(order.orderSourceType == OrderSourceType.Permanent)
      fileOrder += toInt(order.orderSourceType == OrderSourceType.FileOrder)
      if (order.isSuspended) {
        suspended += 1
      } else {
        notPlanned  += toInt(order.isOrderProcessingState[NotPlanned.type])
        planned     += toInt(order.isOrderProcessingState[Planned])
        due         += toInt(order.isOrderProcessingState[Due])
        started     += toInt(order.isOrderProcessingState[Started])
        inTask      += toInt(order.isOrderProcessingState[InTask])
        inTaskProcess += toInt(order.isOrderProcessingState[InTaskProcess])
        occupiedByClusterMember += toInt(order.isOrderProcessingState[OccupiedByClusterMember])
        setback     += toInt(order.isOrderProcessingState[Setback])
        waitingForResource += toInt(order.isOrderProcessingState[WaitingForResource.type])
        blacklisted += toInt(order.isBlacklisted)
      }
    }

    def toImmutable = JocOrderStatistics(
      total = total,
      notPlanned = notPlanned,
      planned = planned,
      due = due,
      started = started,
      inTask = inTask,
      inTaskProcess = inTaskProcess,
      occupiedByClusterMember = occupiedByClusterMember,
      setback = setback,
      waitingForResource = waitingForResource,
      suspended = suspended,
      blacklisted = blacklisted,
      permanent = permanent,
      fileOrder = fileOrder)
  }

  private def toInt(b: Boolean) = if (b) 1 else 0
}
