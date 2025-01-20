package js7.data.board

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.orderingBy
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import scala.jdk.OptionConverters.*

/** Globally unique NoticeId. */
final case class NoticeId(planId: PlanId, boardPath: BoardPath, noticeKey: NoticeKey):

  def plannedNoticeKey: PlannedNoticeKey =
    PlannedNoticeKey(planId, noticeKey)


object NoticeId:

  def checked(planId: PlanId, boardPath: BoardPath, noticeKey: NoticeKey): Checked[NoticeId] =
    PlannedNoticeKey.checked(planId, noticeKey).map: _ =>
      new NoticeId(planId, boardPath, noticeKey)

  @javaApi
  def of(planId: PlanId, boardPath: BoardPath, noticeKey: NoticeKey): NoticeId =
    new NoticeId(planId.nn, boardPath.nn, noticeKey.nn)

  given Ordering[NoticeId] = orderingBy(_.planId, _.boardPath, _.noticeKey)

  given Encoder[NoticeId] = o =>
    if o.planId.isGlobal then
      Json.fromValues:
        Seq(o.boardPath.asJson) ++
          o.noticeKey.nonEmpty ? o.noticeKey.asJson
    else
      Json.fromValues:
        Seq(
          o.planId.planSchemaId.asJson,
          o.planId.planKey.asJson,
          o.boardPath.asJson
        ) ++ o.noticeKey.nonEmpty ? o.noticeKey.asJson


  given Decoder[NoticeId] = c =>
    c.as[Vector[String]].flatMap: vec =>
      toDecoderResult(c.history):
        if vec.size <= 2 then // global Plan?
          for
            boardPath <- vec.checked(0).flatMap(BoardPath.checked)
            noticeKey <- vec.get(1).fold(Checked(NoticeKey.empty))(NoticeKey.checked)
            noticeId <- checked(PlanId.Global, boardPath, noticeKey)
          yield
            noticeId
        else
          for
            planSchemaId <- vec.checked(0).flatMap(PlanSchemaId.checked)
            planKey <- vec.checked(1).flatMap(PlanKey.checked)
            boardPath <- vec.checked(2).flatMap(BoardPath.checked)
            noticeKey <- vec.get(3).fold(Checked(NoticeKey.empty))(NoticeKey.checked)
            noticeId <- checked(planSchemaId / planKey, boardPath, noticeKey)
          yield
            noticeId
