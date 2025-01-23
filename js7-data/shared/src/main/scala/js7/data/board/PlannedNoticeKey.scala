package js7.data.board

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import javax.annotation.Nonnull
import js7.base.circeutils.CirceUtils.*
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.{Checked, ProblemException}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.orderingBy
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.ifNonEmpty
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import org.jetbrains.annotations.TestOnly
import scala.collection.View
import scala.jdk.OptionConverters.*

/** A pair of PlanId and NoticeKey.
  *
  * A PlannedNoticeKey combined with a BoardPath is a NoticeId.
  *
  * A PlannedNoticeKey denotes a NoticeId of a PlannabeBoard or a GlobalBoard.
  */
final case class PlannedNoticeKey private[board](planId: PlanId, noticeKey: NoticeKey):

  override def toString =
    s"PlannedNoticeKey:${planId.shortString}/${noticeKey.nonEmpty ?? noticeKey.toString}"


object PlannedNoticeKey:

  /** Make a PlannedNoticeKey for a GlobalBoard. */
  @TestOnly @throws[ProblemException]
  def apply(planId: PlanId, noticeKey: NoticeKey): PlannedNoticeKey =
    checked(planId, noticeKey).orThrow

  def global(string: String): Checked[PlannedNoticeKey] =
    NoticeKey.checked(string).flatMap(global)

  def global(noticeKey: NoticeKey): Checked[PlannedNoticeKey] =
    checked(PlanId.Global, noticeKey)

  @deprecated
  def empty(planId: PlanId): PlannedNoticeKey =
    assertThat(!planId.isGlobal)
    new PlannedNoticeKey(planId, NoticeKey.empty)

  def checked(planId: PlanId, noticeKey: String): Checked[PlannedNoticeKey] =
    NoticeKey.checked(noticeKey).flatMap(checked(planId, _))

  def checked(planId: PlanId, noticeKey: NoticeKey): Checked[PlannedNoticeKey] =
    if planId.isGlobal && noticeKey.isEmpty then
      Left(EmptyStringProblem("PlannedNoticeKey"))
    else
      Right(new PlannedNoticeKey(planId, noticeKey))

  @Nonnull @throws[RuntimeException]
  def of(noticeKey: String): PlannedNoticeKey =
    global(noticeKey).orThrow

  @Nonnull @throws[RuntimeException]
  def of(planId: PlanId, noticeKey: String): PlannedNoticeKey =
    checked(planId, noticeKey).orThrow

  given Ordering[PlannedNoticeKey] = orderingBy(_.planId, _.noticeKey)

  given Encoder[PlannedNoticeKey] = o =>
    if o.planId.isGlobal then
      o.noticeKey.asJson
    else
      Json.fromValues:
        View(o.planId.planSchemaId.asJson, o.planId.planKey.asJson) ++
          o.noticeKey.string.ifNonEmpty.map(_.asJson)

  given Decoder[PlannedNoticeKey] = c =>
    c.value.asString match
      case Some(string) =>
        NoticeKey.checked(string).flatMap(PlannedNoticeKey.global).toDecoderResult(c.history)
      case None =>
        c.as[Vector[String]].flatMap: seq =>
          locally:
            for
              planSchemaId <- seq.checked(0).flatMap(PlanSchemaId.checked)
              planKey <- seq.checked(1).flatMap(PlanKey.checked)
              noticeKey <- seq.get(2).fold(Checked(NoticeKey.empty))(NoticeKey.checked)
              plannedNoticeKey <- checked(PlanId(planSchemaId, planKey), noticeKey)
            yield
              plannedNoticeKey
          .toDecoderResult(c.history)


object GlobalNoticeKey:

  @TestOnly
  def apply(noticeKey: String): PlannedNoticeKey =
    apply(NoticeKey(noticeKey))

  @TestOnly
  def apply(noticeKey: NoticeKey): PlannedNoticeKey =
    PlannedNoticeKey(PlanId.Global, noticeKey)

  def checked(noticeKey: String): Checked[PlannedNoticeKey] =
    NoticeKey.checked(noticeKey).flatMap(checked)

  def checked(noticeKey: NoticeKey): Checked[PlannedNoticeKey] =
    PlannedNoticeKey.checked(PlanId.Global, noticeKey)
