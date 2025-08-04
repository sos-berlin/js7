package js7.data.board

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.orderingBy
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import scala.annotation.nowarn

/** Globally unique NoticeId. */
final case class NoticeId(planId: PlanId, boardNoticeKey: BoardNoticeKey):

  export planId.{planSchemaId, planKey}
  export boardNoticeKey.{boardPath, noticeKey}

  def plannedBoardId: PlannedBoardId =
    planId / boardPath

  def plannedNoticeKey: PlannedNoticeKey =
    planId / noticeKey

  override def toString =
    s"Notice:${planId.shortString}╱${boardNoticeKey.toShortString}"


object NoticeId:

  @javaApi
  @nowarn("msg=Unnecessary .nn: qualifier")
  def of(planId: PlanId, boardPath: BoardPath, noticeKey: NoticeKey): NoticeId =
    new NoticeId(planId.nn, boardPath.nn / noticeKey.nn)

  given Ordering[NoticeId] = orderingBy(_.planId, _.boardPath, _.noticeKey)

  given Encoder[NoticeId] = o =>
    if o.planId.isGlobal then
      Json.fromValues:
        Seq(o.boardPath.asJson) ++
          o.noticeKey.nonEmpty ? o.noticeKey.asJson
    else
      Json.fromValues:
        Seq(
          o.planSchemaId.asJson,
          o.planKey.asJson,
          o.boardPath.asJson
        ) ++ o.noticeKey.nonEmpty ? o.noticeKey.asJson


  given Decoder[NoticeId] = c =>
    c.as[Vector[String]].flatMap: vec =>
      toDecoderResult(c.history):
        if vec.size <= 2 then // global Plan?
          for
            boardPath <- vec.checked(0).flatMap(BoardPath.checked)
            noticeKey <- vec.get(1).fold(Checked(NoticeKey.empty))(NoticeKey.checked)
          yield
            PlanId.Global / boardPath / noticeKey
        else
          for
            planSchemaId <- vec.checked(0).flatMap(PlanSchemaId.checked)
            planKey <- vec.checked(1).flatMap(PlanKey.checked)
            boardPath <- vec.checked(2).flatMap(BoardPath.checked)
            noticeKey <- vec.get(3).fold(Checked(NoticeKey.empty))(NoticeKey.checked)
          yield
            planSchemaId / planKey / boardPath / noticeKey
