package js7.data.plan

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import js7.base.circeutils.CirceUtils.toDecoderResult
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.orderingBy
import js7.data.board.{BoardNoticeKey, BoardPath, NoticeId, NoticeKey, PlannedBoardId, PlannedNoticeKey}
import js7.data.plan.PlanId.*
import js7.data.value.expression.Expression.{ListExpr, StringConstant}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{ListValue, StringValue}
import scala.annotation.static

/** Identifies a 'Plan', a thought thing which exists only as this `PlanId`. */
final case class PlanId(planSchemaId: PlanSchemaId, planKey: PlanKey):

  def isGlobal: Boolean =
    this == Global

  def /(boardNoticeKey: BoardNoticeKey): NoticeId =
    NoticeId(this, boardNoticeKey)

  def /(boardPath: BoardPath): PlannedBoardId =
    PlannedBoardId(this, boardPath)

  def /(noticeKey: NoticeKey): PlannedNoticeKey =
    PlannedNoticeKey(this, noticeKey)

  def checked: Checked[this.type] =
    if planSchemaId.isGlobal && planKey != PlanKey.Global then
      Left(Problem:
        s"Invalid $toString: The only PlanKey in the global PlanSchema is ${PlanKey.Global}))")
    else
      Right(this)

  def toExpression: Expression =
    if isGlobal then
      ListExpr.empty
    else
      ListExpr.of(
        StringConstant(planSchemaId.string),
        StringConstant(planKey.string))

  override def toString =
    s"Plan:$shortString"

  def shortString: String =
    if isGlobal then
      "Global"
    else
      s"${planSchemaId.string}╱${planKey.string}"


object PlanId:

  @static final val Global: PlanId =
    PlanSchemaId.Global / PlanKey.Global
  @static val GlobalPlanIdExpr: Expression = PlanId.Global.toExpression

  given Ordering[PlanId] = orderingBy(_.planSchemaId, _.planKey)

  given Encoder[PlanId] =
    case PlanId.Global => Json.fromValues(Nil)
    case PlanId(planSchemaId, planKey) => Json.arr(planSchemaId.asJson, planKey.asJson)

  given Decoder[PlanId] = c =>
    c.as[Vector[String]].flatMap:
      case Vector() => Right(PlanId.Global)
      case Vector(planSchemaId, planKey) =>
        toDecoderResult(c.history):
          for
            planSchemaId <- PlanSchemaId.checked(planSchemaId)
            planKey <- PlanKey.checked(planKey)
          yield
            PlanId(planSchemaId, planKey)
      case _ =>
        Left(DecodingFailure("Invalid PlanId", c.history))

  /** @param scope is expected to contain an Order Scope. */
  def evalPlanIdExpr(expr: Expression, scope: Scope): Checked[PlanId] =
    expr.eval(using scope).flatMap:
      case ListValue(Vector()) => Right(PlanId.Global)
      case ListValue(Vector(StringValue(planSchemaId), StringValue(planKey))) =>
        for
          planSchemaId <- PlanSchemaId.checked(planSchemaId)
          planKey <- PlanKey.checked(planKey)
        yield planSchemaId / planKey
      case other => Left(Problem.pure:
        s"""Invalid PlanId expression result (something like ["PlanSchemaId", "PlanKey"] was expected): $other""")
