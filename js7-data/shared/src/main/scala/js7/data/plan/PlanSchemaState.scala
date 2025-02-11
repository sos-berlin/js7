package js7.data.plan

import cats.syntax.traverse.*
import fs2.Stream
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.typed.Subtype
import js7.base.fs2utils.StreamExtensions.:+
import js7.base.problem.Checked.RichCheckedIterable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.PlanIsClosedProblem
import js7.data.board.{BoardPath, NoticeSnapshot, PlannedBoard}
import js7.data.item.UnsignedSimpleItemState
import js7.data.order.{Order, OrderId}
import js7.data.plan.PlanSchemaState.*
import js7.data.value.NamedValues
import js7.data.value.expression.scopes.NamedValueScope
import scala.collection.immutable.Map.Map1
import scala.collection.{View, immutable}

final case class PlanSchemaState(
  item: PlanSchema,
  namedValues: NamedValues,
  toPlan: Map[PlanKey, Plan])
extends UnsignedSimpleItemState:

  protected type Self = PlanSchemaState

  val companion: PlanSchemaState.type = PlanSchemaState
  private lazy val namedValuesScope = NamedValueScope.simple(namedValues orElse item.namedValues)

  export item.isGlobal

  inline def planSchema: PlanSchema =
    item

  def path: PlanSchemaId =
    planSchema.id

  def id: PlanSchemaId =
    planSchema.id

  def estimatedSnapshotSize: Int =
    (if isGlobal then 0 else item.estimatedSnapshotSize + 1)
      + toPlan.values.view.map(_.estimatedSnapshotSize).sum

  override def toSnapshotStream
  : Stream[fs2.Pure, PlanSchema | Snapshot | Plan.Snapshot | NoticeSnapshot] =
    Stream.fromOption(!isGlobal ? ()).flatMap: _ =>
      item.toSnapshotStream :+ Snapshot(path, namedValues)
    .append:
      Stream.iterable(toPlan.values).flatMap(_.toSnapshotStream)

  override def toStringStream: Stream[fs2.Pure, String] =
    Stream.emit(s"PlanSchemaState($path)") ++
      Stream.iterable(namedValues.toVector.sortBy(_._1)).map((k, v) => s"  - $k=$v") ++
      Stream.iterable(toPlan.values.toVector.sorted).flatMap(_.toStringStream).map(o => s"  $o")

  override def toString: String =
    s"PlanSchemaState($path, ${
      namedValues.toVector.sortBy(_._1).view.map((k, v) => s"$k=$v").mkString("{", ", ", "}")
    } ${
      toPlan.values.toVector.sorted.mkString("{", ", ", "}")
    })"

  def recover(snapshot: Snapshot): PlanSchemaState =
    copy(namedValues = snapshot.namedValues)

  def updateNamedValues(namedValues: NamedValues): Checked[PlanSchemaState] =
    copy(namedValues = namedValues).recalculateIsClosedValues

  private def recalculateIsClosedValues: Checked[PlanSchemaState] =
    toPlan.values.map: plan =>
      calculatePlanIsClosed(plan.id.planKey).map: isClosed =>
        plan.copy(isClosed = isClosed)
    .combineProblems
    .map: updatedPlans =>
      copy(
        toPlan = toPlan ++ updatedPlans.toKeyedMap(_.id.planKey))

  /** Returns Right(()) iff this PlanSchema is unused. */
  def checkIsDeletable: Checked[Unit] =
    val usedPlans = toPlan.values.filter(_.hasOrders)
    usedPlans.isEmpty !!
      Problem:
        s"$id cannot be deleted because it is in use by ${
          usedPlans.toVector.sorted.map: plan =>
            if plan.orderIds.size == 1 then
              s"${plan.id.planKey} with ${plan.orderIds.head}"
            else
              s"${plan.id.planKey} with ${plan.orderIds.size} orders"
          .mkString(", ")
        }"

  def checkIsOpen(planKey: PlanKey): Checked[Unit] =
    for
      isClosed <- isClosed(planKey)
      _ <- !isClosed !! PlanIsClosedProblem(planSchema.id / planKey)
    yield
      ()

  private def isClosed(planKey: PlanKey): Checked[Boolean] =
    toPlan.get(planKey) match
      case Some(plan) => Right(plan.isClosed)
      case None => calculatePlanIsClosed(planKey)

  private def calculatePlanIsClosed(planKey: PlanKey): Checked[Boolean] =
    planSchema.isClosed(planKey, namedValuesScope)

  def orderIds: View[OrderId] =
    toPlan.values.view.flatMap(_.orderIds)

  def updateItem(item: PlanSchema): Checked[PlanSchemaState] =
    if isGlobal then
      Left(Problem("The Global PlanSchema cannot be changed"))
    else
      Right(copy(item = item))

  def addOrder(planKey: PlanKey, orderId: OrderId): Checked[PlanSchemaState] =
    addOrderIds(Map1(planKey, Set(orderId)))

  def removeOrderId(planKey: PlanKey, orderId: OrderId): PlanSchemaState =
    removeOrderIds(Map1(planKey, Set(orderId)))

  private def addOrderIds(planToOrders: Map[PlanKey, Set[OrderId]]): Checked[PlanSchemaState] =
    planToOrders.toVector.traverse: (planKey, orderIds) =>
      plan(planKey).map:
        _.addOrders(orderIds)
    .map: plans =>
      copy(toPlan = toPlan ++ plans.toKeyedMap(_.id.planKey))

  private def removeOrderIds(planToOrders: Map[PlanKey, Set[OrderId]]): PlanSchemaState =
    val plans = planToOrders.flatMap: (planKey, orderIds) =>
      toPlan.get(planKey).map: plan =>
        plan.removeOrders(orderIds)
    val (emptyPlans, nonEmptyPlans) = plans.partition(_.isEmpty)
    copy(toPlan = toPlan
      -- emptyPlans.map(_.id.planKey)
      ++ nonEmptyPlans.map(o => o.id.planKey -> o))

  def updatePlannedBoard(planKey: PlanKey, boardPath: BoardPath)
    (updateItemStates: PlannedBoard => Checked[PlannedBoard])
  : Checked[PlanSchemaState] =
    for
      plan <- plan(planKey)
      plannedBoard <- updateItemStates(plan.plannedBoard(boardPath))
      updatedPlan = plan.copy(
        toPlannedBoard =
          plan.toPlannedBoard.updatedWith(boardPath): _ =>
            !plannedBoard.isEmpty ? plannedBoard)
    yield
      copy(
        toPlan = toPlan.updatedWith(planKey): _ =>
          !updatedPlan.isEmpty ? updatedPlan)

  /** Creates an empty Plan if it does not exist.
    *
    * @return Left(problem) if `PlanSchema.planIsClosedFunction` fails. */
  def plan(planKey: PlanKey): Checked[Plan] =
    toPlan.get(planKey).map(Right(_)).getOrElse:
      calculatePlanIsClosed(planKey).flatMap: isClosed =>
        Plan.checked(id / planKey, isClosed = isClosed)

  def removeBoard(boardPath: BoardPath): PlanSchemaState =
    copy(toPlan =
      toPlan.values.flatMap: plan =>
        plan.removeBoard(boardPath)
      .map(o => o.id.planKey -> o)
      .toMap)

  def planIds: Iterable[PlanKey] =
    toPlan.keys

  def plans: Iterable[Plan] =
    toPlan.values


object PlanSchemaState extends UnsignedSimpleItemState.Companion[PlanSchemaState]:

  type Key = PlanSchemaId
  type Item = PlanSchema

  def initialGlobal: PlanSchemaState =
    PlanSchema.Global.toInitialItemState

  def recoverPlanSchemaStatesFromOrders(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => PlanSchemaState)
  : Checked[Seq[PlanSchemaState]] =
    for
      planSchemaStates1 <- recoverPlannedOrdersFromOrders(orders, toPlanSchemaState)
      planSchemaStates2 <- recoverExpectedNoticesFromOrders(orders,
        planSchemaStates1.toKeyedMap(_.id) orElse (o => toPlanSchemaState(o)))
    yield
      (planSchemaStates2 ++ planSchemaStates1).distinctBy(_.id)

  private def recoverPlannedOrdersFromOrders(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => PlanSchemaState)
  : Checked[Seq[PlanSchemaState]] =
    orders.toVector
      .groupBy(_.planId.planSchemaId)
      .toVector
      .traverse: (planSchemaId, orders) =>
        val planSchemaState = toPlanSchemaState(planSchemaId)
        orders.groupBy(_.planId.planKey).toVector.traverse: (planKey, orders) =>
          planSchemaState.plan(planKey).map: plan =>
            plan.copy(orderIds = plan.orderIds ++ orders.map(_.id))
        .map: plans =>
          planSchemaState.copy(
            toPlan = planSchemaState.toPlan ++ plans.toKeyedMap(_.id.planKey))

  private def recoverExpectedNoticesFromOrders(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => PlanSchemaState)
  : Checked[Seq[PlanSchemaState]] =
    orders.view
      .flatMap(_.ifState[Order.ExpectingNotices])
      .flatMap(o => o.state.noticeIds.map(_ -> o.id))
      .toVector.groupBy(_._1.planSchemaId)
      .toVector.traverse: (planSchemaId, noticeToOrder) =>
        val planSchemaState = toPlanSchemaState(planSchemaId)
        noticeToOrder.groupBy(_._1.planKey)
          .toVector.traverse: (planKey, noticeToOrder) =>
            planSchemaState.plan(planKey).map: plan =>
              val plannedBoards =
              noticeToOrder.groupBy(_._1.boardPath)
                .toVector.map: (boardPath, noticeToOrder) =>
                    noticeToOrder.view
                      .scanLeft(plan.plannedBoard(boardPath)):
                        case (plannedBoard, (noticeId, orderId)) =>
                          plannedBoard.addExpectation(noticeId.noticeKey, orderId)
                      .last
              plan.copy(
                toPlannedBoard = plan.toPlannedBoard ++ plannedBoards.map(o => o.boardPath -> o))
          .map: plans =>
            planSchemaState.copy(toPlan = planSchemaState.toPlan ++
              plans.map(plan => plan.planKey -> plan))

  def addOrderIds(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => Checked[PlanSchemaState])
  : Checked[Seq[PlanSchemaState]] =
    updatedSchemaStates(orders, toPlanSchemaState)
      .flatMap:
        _.traverse: (planSchemaState, planToOrders) =>
          planSchemaState.addOrderIds(planToOrders)

  def removeOrderIds(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => Checked[PlanSchemaState])
  : Checked[Seq[PlanSchemaState]] =
    updatedSchemaStates(orders, toPlanSchemaState)
      .map(_.map: (planSchemaState, planToOrders) =>
        planSchemaState.removeOrderIds(planToOrders))

  private def updatedSchemaStates(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => Checked[PlanSchemaState])
  : Checked[Seq[(PlanSchemaState, Map[PlanKey, Set[OrderId]])]] =
    if orders.isEmpty then
      Right(Nil)
    else
      orders.iterator
        .map(o => o.planId -> o.id)
        .to(Set)
        .groupMap(_._1.planSchemaId): (planId, orderId) =>
          planId.planKey -> orderId
        .view
        .mapValues:
          _.groupMap(_._1)(_._2)
        .toVector
        .traverse: (planSchemaId, planKeyToOrderIds) =>
          toPlanSchemaState(planSchemaId).map(_ -> planKeyToOrderIds)


  final case class Snapshot(id: PlanSchemaId, namedValues: NamedValues):
    override def productPrefix = s"PlanSchemaState.Snapshot"

  val subtype: Subtype[Snapshot] =
    Subtype.named[Snapshot](deriveCodecWithDefaults, "PlanSchemaState")
