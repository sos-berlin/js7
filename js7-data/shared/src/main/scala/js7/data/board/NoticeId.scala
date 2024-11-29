package js7.data.board

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import javax.annotation.Nonnull
import js7.base.circeutils.CirceUtils.*
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.{Checked, ProblemException}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.ifNonEmpty
import js7.data.plan.{PlanId, PlanKey, PlanTemplateId}
import org.jetbrains.annotations.TestOnly
import scala.collection.View
import scala.jdk.OptionConverters.*

final case class NoticeId private(noticeKey: NoticeKey, planId: PlanId):

  override def toString =
    s"NoticeId${noticeKey.nonEmpty ?? s":$noticeKey"}${!planId.isGlobal ?? s":${planId.shortString}"}"


object NoticeId:

  /** Make a NoticeId for a GlobalBoard. */
  @TestOnly @throws[ProblemException]
  def apply(string: String): NoticeId =
    global(string).orThrow

  @TestOnly @throws[ProblemException]
  def apply(noticeKey: NoticeKey, planId: PlanId): NoticeId =
    checked(noticeKey, planId).orThrow

  def global(string: String): Checked[NoticeId] =
    NoticeKey.checked(string).flatMap(global)

  def global(noticeKey: NoticeKey): Checked[NoticeId] =
    checked(noticeKey, PlanId.Global)

  def planned(planId: PlanId): NoticeId =
    assertThat(!planId.isGlobal)
    new NoticeId(NoticeKey.empty, planId)

  def checked(noticeKey: NoticeKey, planId: PlanId): Checked[NoticeId] =
    if planId.isGlobal && noticeKey.isEmpty then
      Left(EmptyStringProblem("NoticeId"))
    else
      Right(new NoticeId(noticeKey, planId))

  @Nonnull
  def of(noticeKey: String): NoticeId =
    global(NoticeKey(noticeKey)).orThrow

  @Nonnull
  def of(planId: PlanId, noticeKey: String): NoticeId =
    checked(NoticeKey(noticeKey), planId).orThrow

  given Encoder[NoticeId] = o =>
    if o.planId.isGlobal then
      o.noticeKey.asJson
    else
      Json.fromValues:
        View(o.planId.planTemplateId.asJson, o.planId.planKey.asJson) ++
          o.noticeKey.string.ifNonEmpty.map(_.asJson)

  given Decoder[NoticeId] = c =>
    c.value.asString match
      case Some(string) =>
        NoticeKey.checked(string).flatMap(NoticeId.global).toDecoderResult(c.history)
      case None =>
        c.as[Vector[String]].flatMap: seq =>
          locally:
            for
              planTemplateId <- seq.checked(0).flatMap(PlanTemplateId.checked)
              planKey <- seq.checked(1).flatMap(PlanKey.checked)
              noticeKey <- seq.get(2).fold(Checked(NoticeKey.empty))(NoticeKey.checked)
              noticeId <- checked(noticeKey, PlanId(planTemplateId, planKey))
            yield
              noticeId
          .toDecoderResult(c.history)
