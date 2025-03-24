package js7.data.plan

import cats.syntax.traverse.*
import fs2.Stream
import io.circe.generic.semiauto
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Codec, Decoder, Encoder}
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.Subtype
import js7.base.fs2utils.StreamExtensions.:+
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime
import js7.base.time.ScalaTime.ZeroDuration
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.PlanIsClosedProblem
import js7.data.board.{BoardPath, NoticeSnapshot, PlannedBoard}
import js7.data.item.UnsignedSimpleItemState
import js7.data.order.{Order, OrderId}
import js7.data.plan.PlanSchemaEvent.PlanSchemaChanged
import js7.data.plan.PlanSchemaState.*
import js7.data.plan.PlanStatus.{Closed, Deleted, Finished, Open}
import js7.data.value.NamedValues
import js7.data.value.expression.scopes.NamedValueScope
import scala.collection.immutable.Map.Map1
import scala.collection.{View, immutable}
import scala.concurrent.duration.FiniteDuration

final case class PlanSchemaState(
  item: PlanSchema,
  finishedPlanRetentionPeriod: FiniteDuration,
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
      item.toSnapshotStream :+ Snapshot(path, finishedPlanRetentionPeriod, namedValues)
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

  def updateItem(item: PlanSchema): Checked[PlanSchemaState] =
    if isGlobal then
      Left(Problem("The Global PlanSchema cannot be changed"))
    else
      copy(item = item).removeDicardablePlans

  def recover(snapshot: Snapshot): PlanSchemaState =
    copy(
      finishedPlanRetentionPeriod = snapshot.finishedPlanRetentionPeriod,
      namedValues = snapshot.namedValues)

  def applyEvent(event: PlanSchemaEvent): Checked[PlanSchemaState] =
    event match
      case PlanSchemaChanged(finishedPlanRetentionPeriod, namedValues) =>
        val planSchemaState = this
        for
          planSchemaState <- namedValues.fold(Checked(planSchemaState)): namedValues =>
            planSchemaState.copy(namedValues = namedValues)
              .removeDicardablePlans
          planSchemaState <- finishedPlanRetentionPeriod.fold(Checked(planSchemaState)):
            planSchemaState.updatefinishedPlanRetentionPeriod
        yield
          planSchemaState

  def applyPlanEvent(planKey: PlanKey, event: PlanEvent): Checked[PlanSchemaState] =
    for
      plan <- plan(planKey)
      plan <- plan.applyEvent(event)
    yield
      updatePlans(plan :: Nil)

  def updatefinishedPlanRetentionPeriod(duration: FiniteDuration): Checked[PlanSchemaState] =
    Right(copy(finishedPlanRetentionPeriod = duration))

  def orderIds: View[OrderId] =
    toPlan.values.view.flatMap(_.orderIds)

  def addOrder(planKey: PlanKey, orderId: OrderId): Checked[PlanSchemaState] =
    addOrders(Map1(planKey, Set(orderId)))

  private def addOrders(planToOrders: Map[PlanKey, Set[OrderId]]): Checked[PlanSchemaState] =
    planToOrders.toVector.traverse: (planKey, orderIds) =>
      plan(planKey).flatMap:
        _.addOrders(orderIds)
    .map(updatePlans)

  def removeOrder(planKey: PlanKey, orderId: OrderId): PlanSchemaState =
    removeOrders(Map1(planKey, Set(orderId)))

  private def removeOrders(planToOrders: Map[PlanKey, Set[OrderId]]): PlanSchemaState =
    updatePlans:
      planToOrders.flatMap: (planKey, orderIds) =>
        toPlan.get(planKey).map: plan =>
          plan.removeOrders(orderIds)

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
      updatePlans(updatedPlan :: Nil)

  private def isDiscardableIgnoringProblem(plan: Plan): Boolean =
    isDiscardable(plan).onProblemHandle: problem =>
      logger.error(s"${plan.id} unknownPlanIsClosedFunction failed: $problem")
      false

  /** Whether the Plan can be removed from toPlan without change of semantics.
    *
    * A Plan isDiscardable when it isDiscardableCandidate (empty, and Open or Deleted), and
    * evalUnknownPlanIsClosed evaluates to an equivalent value.
    */
  private[plan] def isDiscardable(plan: Plan): Checked[Boolean] =
    import plan.id.planKey
    plan.status match
      case Open | Deleted =>
        // Check whether we may forget the Plan.
        // evalUnknownPlanIsClosed returns the default Open/Deleted Status for forgotten Plans.
        if !plan.isDiscardableCandidate then
          Right(false)
        else
          evalUnknownPlanIsClosed(planKey).map(_ == (plan.status == Deleted))
      case Closed | _: Finished => Right(false)

  /** Returns Right(()) iff this PlanSchema is unused. */
  def checkIsDeletable: Checked[Unit] =
    val usedPlans = toPlan.values.filter(_.hasOrders)
    usedPlans.isEmpty !! Problem:
      s"$id cannot be deleted because it is in use by ${
        usedPlans.toVector.sorted.map: plan =>
          s"${plan.id.planKey} with ${plan.orderIds.mkStringLimited(3)}"
        .mkString(", ")
      }"

  def checkIsOpen(planKey: PlanKey): Checked[Unit] =
    for
      isClosed <- isClosed(planKey)
      _ <- !isClosed !! PlanIsClosedProblem(planSchema.id / planKey)
    yield
      ()

  private def isClosed(planKey: PlanKey): Checked[Boolean] =
    plan(planKey).map(_.isClosed)

  /** Creates an empty Plan with status computed by unknownPlanIsClosedFunction, if Plan does not exist.
    *
    * @return Left(problem) if `PlanSchema.unknownPlanIsClosedFunction` fails. */
  def plan(planKey: PlanKey): Checked[Plan] =
    toPlan.get(planKey).map(Right(_)).getOrElse:
      evalUnknownPlanIsClosed(planKey).flatMap: isClosed =>
        Plan.checked(id / planKey, if isClosed then Deleted else Open)

  private def evalUnknownPlanIsClosed(planKey: PlanKey): Checked[Boolean] =
    planSchema.evalUnknownPlanIsClosed(planKey, namedValuesScope)

  def removeBoard(boardPath: BoardPath): PlanSchemaState =
    updatePlans:
      toPlan.values.map: plan =>
        plan.removeBoard(boardPath)

  private def removeDicardablePlans: Checked[PlanSchemaState] =
    toPlan.values.toVector.traverse: plan =>
      isDiscardable(plan).map: isDiscardable =>
        if isDiscardable then logRemovedPlan(plan)
        !isDiscardable ? plan
    .map(_.flatten)
    .map: plans =>
      copy(toPlan = plans.toKeyedMap(_.id.planKey))

  private def updatePlans(plans: Iterable[Plan]): PlanSchemaState =
    val (removablePlans, updatedPlans) = plans.partition(isDiscardableIgnoringProblem)
    removablePlans.foreach(logRemovedPlan)
    logger.whenTraceEnabled(updatedPlans.foreach(logUpdatedPlan))
    copy(toPlan = toPlan
      -- removablePlans.map(_.planKey)
      ++ updatedPlans.map(o => o.id.planKey -> o))

  private def logUpdatedPlan(plan: Plan): Unit =
    if !toPlan.contains(plan.planKey) then
      logger.trace(s"Add $plan")

  private def logRemovedPlan(plan: Plan): Unit =
    logger.trace(s"Remove $plan")

  def planIds: Iterable[PlanKey] =
    toPlan.keys

  def plans: Iterable[Plan] =
    toPlan.values


object PlanSchemaState extends UnsignedSimpleItemState.Companion[PlanSchemaState]:

  type Key = PlanSchemaId
  type Item = PlanSchema

  private val logger = Logger[this.type ]

  def initialGlobal: PlanSchemaState =
    PlanSchema.Global.toInitialItemState

  def recoverPlanSchemaStatesFromOrders(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => Checked[PlanSchemaState])
  : Checked[Seq[PlanSchemaState]] =
    for
      planSchemaStates1 <- recoverPlannedOrdersFromOrders(orders, toPlanSchemaState)
      toPlanSchemaState1 = planSchemaStates1.toKeyedMap(_.id)
      planSchemaStates2 <- recoverExpectedNoticesFromOrders(
        orders,
        planSchemaId => toPlanSchemaState1.get(planSchemaId)
          .fold(toPlanSchemaState(planSchemaId))(Right(_)))
    yield
      (planSchemaStates2 ++ planSchemaStates1).distinctBy(_.id)

  private def recoverPlannedOrdersFromOrders(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => Checked[PlanSchemaState])
  : Checked[Seq[PlanSchemaState]] =
    orders.toVector
      .groupBy(_.planId.planSchemaId)
      .toVector
      .traverse: (planSchemaId, orders) =>
        toPlanSchemaState(planSchemaId).flatMap: planSchemaState =>
          orders.groupBy(_.planId.planKey).toVector.traverse: (planKey, orders) =>
            planSchemaState.plan(planKey).map: plan =>
              plan.copy(orderIds = plan.orderIds ++ orders.map(_.id))
          .map: plans =>
            planSchemaState.copy(
              toPlan = planSchemaState.toPlan ++ plans.toKeyedMap(_.id.planKey))

  private def recoverExpectedNoticesFromOrders(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => Checked[PlanSchemaState])
  : Checked[Seq[PlanSchemaState]] =
    orders.view
      .flatMap(_.ifState[Order.ExpectingNotices])
      .flatMap(o => o.state.noticeIds.map(_ -> o.id))
      .toVector.groupBy(_._1.planSchemaId)
      .toVector.traverse: (planSchemaId, noticeToOrder) =>
        toPlanSchemaState(planSchemaId).flatMap: planSchemaState =>
          noticeToOrder.groupBy(_._1.planKey).toVector
            .traverse: (planKey, noticeToOrder) =>
              planSchemaState.plan(planKey).map: plan =>
                val plannedBoards =
                  noticeToOrder.groupBy(_._1.boardPath).toVector
                    .map: (boardPath, noticeToOrder) =>
                      noticeToOrder.view.scanLeft(plan.plannedBoard(boardPath)):
                        case (plannedBoard, (noticeId, orderId)) =>
                          plannedBoard.addExpectation(noticeId.noticeKey, orderId)
                      .last
                plan.copy(toPlannedBoard = plan.toPlannedBoard ++
                  plannedBoards.map(o => o.boardPath -> o))
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
          planSchemaState.addOrders(planToOrders)

  def removeOrderIds(
    orders: Iterable[Order[Order.State]],
    toPlanSchemaState: PlanSchemaId => Checked[PlanSchemaState])
  : Checked[Seq[PlanSchemaState]] =
    updatedSchemaStates(orders, toPlanSchemaState)
      .map(_.map: (planSchemaState, planToOrders) =>
        planSchemaState.removeOrders(planToOrders))

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


  final case class Snapshot(
    id: PlanSchemaId,
    finishedPlanRetentionPeriod: FiniteDuration,
    namedValues: NamedValues):

    override def productPrefix = s"PlanSchemaState.Snapshot"

  object Snapshot:
    private val jsonDecoder: Decoder[Snapshot] = c =>
      for
        id <- c.get[PlanSchemaId]("id")
        finishedPlanRetentionPeriod <- c.getOrElse[FiniteDuration]("finishedPlanRetentionPeriod")(ZeroDuration)
        namedValues <- c.get[NamedValues]("namedValues")
      yield
        Snapshot(id, finishedPlanRetentionPeriod, namedValues)

    val subtype: Subtype[Snapshot] =
      Subtype.named[Snapshot](Codec.AsObject.from(jsonDecoder, deriveEncoder), "PlanSchemaState")
