package js7.data.plan

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import js7.base.utils.ScalaUtils.orderingBy
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.{BoardPath, NoticeId, PlannedBoardId}
import js7.data.plan.PlanId.*

/** Identifies a 'Plan', a thought thing which exists only as this `PlanId`. */
final case class PlanId(planSchemaId: PlanSchemaId, planKey: PlanKey):

  def isGlobal: Boolean =
    this == Global

  def noticeId: NoticeId =
    NoticeId.planned(this)

  def /(boardPath: BoardPath): PlannedBoardId =
    PlannedBoardId(this, boardPath)

  override def toString =
    s"Plan:$shortString"

  def shortString =
    s"${planSchemaId.string}${planKey.string.nonEmpty ?? s"/${planKey.string}"}"


object PlanId:

  val Global: PlanId = PlanId(PlanSchemaId.Global, PlanKey.Global)

  given Ordering[PlanId] = orderingBy(_.planSchemaId, _.planKey)

  given Encoder[PlanId] =
    case PlanId(planSchemaId, planKey) => Json.arr(planSchemaId.asJson, planKey.asJson)

  given Decoder[PlanId] = c =>
    c.as[(PlanSchemaId, PlanKey)].map:
      PlanId.apply.tupled
