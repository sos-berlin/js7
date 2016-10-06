package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.order.OrderStatistics._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderStatistics(
  total: Int,
  notPlanned: Int,
  planned: Int,
  due: Int,
  running: Int,
  inTask: Int,
  inProcess: Int,
  setback: Int,
  suspended: Int,
  blacklisted: Int,
  permanent: Int,
  fileOrder: Int)
{
  def +(o: OrderStatistics) = OrderStatistics(
    total = total + o.total,
    notPlanned = notPlanned + o.notPlanned,
    planned = planned + o.planned,
    due = due + o.due,
    running = running + o.running,
    inTask = inTask + o.inTask,
    inProcess = inProcess + o.inProcess,
    setback = setback + o.setback,
    suspended = suspended + o.suspended,
    blacklisted = blacklisted + o.blacklisted,
    permanent = permanent + o.permanent,
    fileOrder = fileOrder + o.fileOrder)

  override def toString = List(
      format("total", total),
      format("notPlanned", notPlanned),
      format("planned", planned),
      format("due", due),
      format("running", running),
      format("inTask", inTask),
      format("inProcess", inProcess),
      format("setback", setback),
      format("suspended", suspended),
      format("blacklisted", blacklisted),
      format("permanent", permanent),
      format("fileOrder", fileOrder))
    .mkString("OrderStatistics(", " ", ")")
}

object OrderStatistics {
  val Zero = OrderStatistics(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  implicit val MyJsonFormat = jsonFormat12(apply)

  private def format(name: String, number: Int) = s"$name=$number"

  final class Mutable(
    var total: Int = 0,
    var notPlanned: Int = 0,
    var planned: Int = 0,
    var due: Int = 0,
    var running: Int = 0,
    var inTask: Int = 0,
    var inProcess: Int = 0,
    var setback: Int = 0,
    var suspended: Int = 0,
    var blacklisted: Int = 0,
    var permanent: Int = 0,
    var fileOrder: Int = 0)
  {
    def toImmutable = OrderStatistics(
      total = total,
      notPlanned = notPlanned,
      planned = planned,
      due = due,
      running = running,
      inTask = inTask,
      inProcess = inProcess,
      setback = setback,
      suspended = suspended,
      blacklisted = blacklisted,
      permanent = permanent,
      fileOrder = fileOrder)
  }
}
