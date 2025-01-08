package js7.data.plan

import cats.syntax.traverse.*
import fs2.{Pure, Stream}
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.typed.Subtype
import js7.base.fs2utils.StreamExtensions.:+
import js7.base.problem.Checked.RichCheckedIterable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.PlanIsClosedProblem
import js7.data.board.{BoardPath, BoardState, NoticeKey, NoticePlace, PlannedBoard, PlannedBoardId}
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
  private lazy val namedValuesScope = NamedValueScope(namedValues)

  inline def planSchema: PlanSchema =
    item

  def path: PlanSchemaId =
    planSchema.id

  def id: PlanSchemaId =
    planSchema.id

  override def toSnapshotStream: fs2.Stream[fs2.Pure, PlanSchema | Snapshot] =
    if isGlobal then
      fs2.Stream.empty
    else
      item.toSnapshotStream :+ Snapshot(path, namedValues)

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

  def isGlobal: Boolean =
    item.isGlobal

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
    Right(copy(item = item))

  def addOrder(planKey: PlanKey, orderId: OrderId): Checked[PlanSchemaState] =
    addOrderIds(Map1(planKey, Set(orderId)))

  def removeOrderId(planKey: PlanKey, orderId: OrderId): PlanSchemaState =
    removeOrderIds(Map1(planKey, Set(orderId)))

  private def addOrderIds(planToOrders: Map[PlanKey, Set[OrderId]]): Checked[PlanSchemaState] =
    planToOrders.toVector.traverse: (planKey, orderIds) =>
      getOrMakePlan(planKey).map:
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

  def updateNoticePlace(planKey: PlanKey, boardPath: BoardPath, noticePlace: NoticePlace)
  : Checked[PlanSchemaState] =
    getOrMakePlan(planKey).map:
      _.updateNoticePlace(boardPath, noticePlace)
    .map: plan =>
      copy(toPlan = toPlan.updated(plan.id.planKey, plan))

  private def getOrMakePlan(planKey: PlanKey): Checked[Plan] =
    toPlan.get(planKey).fold(makePlan(planKey))(Right(_))

  private def makePlan(
    planKey: PlanKey,
    orderIds: Set[OrderId] = Set.empty,
    plannedBoards: Iterable[PlannedBoard] = Nil,
  ): Checked[Plan] =
    calculatePlanIsClosed(planKey).map: isClosed =>
      Plan(id / planKey, orderIds, plannedBoards, isClosed = isClosed)

  def deleteNoticePlace(planKey: PlanKey, boardPath: BoardPath, noticeKey: NoticeKey)
  : PlanSchemaState =
    copy(toPlan =
      toPlan.updatedWith(planKey):
        _.flatMap:
          _.deleteNoticePlace(boardPath, noticeKey))

  def deleteBoard(boardPath: BoardPath): PlanSchemaState =
    copy(toPlan =
      toPlan.values.flatMap: plan =>
        plan.deleteBoard(boardPath)
      .map(o => o.id.planKey -> o)
      .toMap)

  def planIds: Iterable[PlanKey] =
    toPlan.keys

  def plans: Iterable[Plan] =
    toPlan.values

  override def toStringStream: Stream[Pure, String] =
    Stream.emit(s"PlanSchemaState($path)") ++
      Stream.iterable(namedValues.toVector.sortBy(_._1)).map((k, v) => s"  - $k=$v") ++
        Stream.iterable(toPlan.values.toVector.sorted).flatMap(_.toStringStream).map(o => s"  $o")

  override def toString: String =
    s"PlanSchemaState($path, ${
      namedValues.toVector.sortBy(_._1).view.map((k, v) => s"$k=$v").mkString("{", ", ", "}")
    } ${
      toPlan.values.toVector.sorted.mkString("{", ", ", "}")
    })"


object PlanSchemaState extends UnsignedSimpleItemState.Companion[PlanSchemaState]:

  type Key = PlanSchemaId
  type Item = PlanSchema

  val Global: PlanSchemaState =
    PlanSchemaState(PlanSchema.Global, namedValues = Map.empty, toPlan = Map.empty)

  def recoverPlans(
    orders: Iterable[Order[Order.State]],
    boardStates: Iterable[BoardState],
    toPlanSchemaState: PlanSchemaId => Checked[PlanSchemaState])
  : Checked[Seq[PlanSchemaState]] =
    val planToOrders: Map[PlanId, Vector[OrderId]] =
      orders.toVector.groupMap(_.planId)(_.id)

    val planToPlannedBoards: Map[PlanId, Iterable[PlannedBoard]] =
      boardStates.view.flatMap: boardState =>
        boardState.idToNotice.values
          .groupBy: noticePlace =>
            PlannedBoardId(noticePlace.planId, boardState.path)
          .map: (plannedBoardId, noticePlaces) =>
            PlannedBoard(plannedBoardId, noticePlaces)
      .groupBy(_.planId)

    (planToOrders.keySet ++ planToPlannedBoards.keys)
      .toVector
      .groupBy(_.planSchemaId)
      .toVector
      .traverse: (planSchemaId, planIds) =>
        toPlanSchemaState(planSchemaId).flatMap: planSchemaState =>
          planIds.traverse: planId =>
            planSchemaState.makePlan(
              planId.planKey,
              orderIds = planToOrders.getOrElse(planId, Set.empty).toSet,
              plannedBoards = planToPlannedBoards.getOrElse(planId, Nil))
          .map: plans =>
            planSchemaState.copy(
              toPlan = plans.toKeyedMap(_.id.planKey))

  def addOrderIds(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => Checked[PlanSchemaState])
  : Checked[Seq[PlanSchemaState]] =
    updatedTemplateStates(orders, toPlanSchemaState)
      .flatMap:
        _.traverse: (planSchemaState, planToOrders) =>
          planSchemaState.addOrderIds(planToOrders)

  def removeOrderIds(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => Checked[PlanSchemaState])
  : Checked[Seq[PlanSchemaState]] =
    updatedTemplateStates(orders, toPlanSchemaState)
      .map(_.map: (planSchemaState, planToOrders) =>
        planSchemaState.removeOrderIds(planToOrders))

  private def updatedTemplateStates(
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


  final case class Snapshot(id: PlanSchemaId, namedValues: NamedValues)

  val subtype: Subtype[Snapshot] =
    Subtype.named[Snapshot](deriveCodecWithDefaults, "PlanSchemaState")
