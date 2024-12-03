package js7.data.plan

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.UnsignedSimpleItemState
import js7.data.order.{Order, OrderId}
import scala.collection.{View, immutable}

final case class PlanTemplateState(
  planTemplate: PlanTemplate,
  toOrderPlan: Map[PlanKey, OrderPlan])
extends UnsignedSimpleItemState:

  protected type Self = PlanTemplateState

  val companion: PlanTemplateState.type = PlanTemplateState

  val item: PlanTemplate =
    planTemplate

  def path: PlanTemplateId =
    planTemplate.id

  def id: PlanTemplateId =
    planTemplate.id

  override def toSnapshotStream: fs2.Stream[fs2.Pure, PlanTemplate] =
    item.toSnapshotStream

  /** Returns Right(()) iff this PlanTemplate is unused. */
  def checkUnused: Checked[Unit] =
    val usedPlans = toOrderPlan.values.filter(_.hasOrders)
    usedPlans.isEmpty !!
      Problem:
        s"$id is in use by ${
          usedPlans.toVector.sorted.map: plan =>
            import plan.{orderIds, planId}
            if orderIds.size == 1 then
              s"${planId.planKey} with ${orderIds.head}"
            else
              s"${planId.planKey} with ${orderIds.size} orders"
          .mkString(", ")
        }"

  def updateItem(item: PlanTemplate): Checked[PlanTemplateState] =
    Left(Problem("Update of PlanTemplate is still not supported"))

  private def addOrders(planToOrders: Map[PlanKey, Set[OrderId]]): PlanTemplateState =
    val updatedPlans = updateOrdersInPlans(planToOrders, _.addOrders(_))
    copy(toOrderPlan = toOrderPlan ++ updatedPlans.map(o => o.planId.planKey -> o))

  private def removeOrders(planToOrders: Map[PlanKey, Set[OrderId]]): PlanTemplateState =
    val updatedPlans = updateOrdersInPlans(planToOrders, _.removeOrders(_))
    val (emptyPlans, nonEmptyPlans) = updatedPlans.partition(_.isEmpty)
    copy(toOrderPlan = toOrderPlan
      -- emptyPlans.map(_.planId.planKey)
      ++ nonEmptyPlans.map(o => o.planId.planKey -> o))

  private def updateOrdersInPlans(
    planToOrders: Map[PlanKey, Set[OrderId]],
    update: (OrderPlan, Set[OrderId]) => OrderPlan)
  : View[OrderPlan] =
    planToOrders.view.map: (planKey, orderIds) =>
      val plan = toOrderPlan.getOrElse(planKey, OrderPlan.initial(id / planKey))
      update(plan, orderIds)


object PlanTemplateState extends UnsignedSimpleItemState.Companion[PlanTemplateState]:

  type Key = PlanTemplateId
  type Item = PlanTemplate

  val Global: PlanTemplateState =
    PlanTemplateState(PlanTemplate.Global, toOrderPlan = Map.empty)

  def recoverOrderPlans(
    orders: Iterable[Order[Order.State]],
    toPlanTemplate: PlanTemplateId => Checked[PlanTemplate])
  : Checked[Seq[PlanTemplateState]] =
    val planToOrders: Map[PlanId, Set[OrderId]] =
      orders.iterator
        .map(o => o.planId -> o.id)
        .toSet
        .groupMap(_._1)(_._2)

    //val planToBoards: Map[PlanId, Set[BoardPath]] =
    //  boardStates.iterator
    //    .flatMap: boardState =>
    //      boardState.idToNotice.keys.map(_.planId -> boardState.path)
    //    .toSet
    //    .groupMap(_._1)(_._2)
    //
    //val unknownPlanIds = planToBoards.keySet -- planToOrders.keySet
    //if unknownPlanIds.nonEmpty then
    //  unknownPlanIds.iterator.foreachWithBracket(Square): (planId, bracket) =>
    //    logger.error(s"${bracket}PlannedBoard for unknown Plan: $planId ${planToBoards(planId)}")
    //  Left(Problem:
    //    s"Illegal state: there are PlannedBoards for ${unknownPlanIds.size} non-existing Plans: ${
    //      unknownPlanIds.take(10).mkString(" ")}")
    //else
    planToOrders.map: (planId, orders) =>
      OrderPlan(planId, orders)
    .groupMap(_.planId.planTemplateId): plan =>
      plan.planId.planKey -> plan
    .view.mapValues(_.toMap)
    .toVector
    .traverse: o =>
      toPlanTemplate(o._1).map: planTemplate =>
        PlanTemplateState(planTemplate, toOrderPlan = o._2)

  def addOrders(
    orders: Iterable[Order[Order.State]],
    toPlanTemplateState: PlanTemplateId => Checked[PlanTemplateState])
  : Checked[Seq[PlanTemplateState]] =
    updatedTemplateStates(orders, toPlanTemplateState)
      .map(_.map: (planTemplateState, planToOrders) =>
        planTemplateState.addOrders(planToOrders))

  def removeOrders(
    orders: Iterable[Order[Order.State]],
    toPlanTemplateState: PlanTemplateId => Checked[PlanTemplateState])
  : Checked[Seq[PlanTemplateState]] =
    updatedTemplateStates(orders, toPlanTemplateState)
      .map(_.map: (planTemplateState, planToOrders) =>
        planTemplateState.removeOrders(planToOrders))

  private def updatedTemplateStates(
    orders: Iterable[Order[Order.State]],
    toPlanTemplateState: PlanTemplateId => Checked[PlanTemplateState])
  : Checked[Seq[(PlanTemplateState, Map[PlanKey, Set[OrderId]])]] =
    if orders.isEmpty then
      Right(Nil)
    else
      orders.iterator
        .map(o => o.planId -> o.id)
        .to(Set)
        .groupMap(_._1.planTemplateId)(o => o._1.planKey -> o._2)
        .view
        .mapValues:
          _.groupMap(_._1)(_._2)
        .toVector
        .traverse: (planItemId, v) =>
          toPlanTemplateState(planItemId).map(_ -> v)
