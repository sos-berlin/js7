package js7.data.board

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import javax.annotation.Nonnull
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked
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
    s"NoticeKey:$toShortString"

  def toShortString: String =
    s"${planId.shortString}╱${noticeKey.toShortString}"


object PlannedNoticeKey:

  /** Make a PlannedNoticeKey for a GlobalBoard. */
  def apply(planId: PlanId, noticeKey: NoticeKey): PlannedNoticeKey =
    new PlannedNoticeKey(planId, noticeKey)

  def checked(planId: PlanId, noticeKey: String): Checked[PlannedNoticeKey] =
    NoticeKey.checked(noticeKey).map(planId / _)

  /** Make a PlannedNoticeKey in the global Plan. */
  @Nonnull @throws[RuntimeException]
  def of(noticeKey: String): PlannedNoticeKey =
    GlobalNoticeKey.checked(noticeKey).orThrow

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
        GlobalNoticeKey.checked(string).toDecoderResult(c.history)
      case None =>
        c.as[Vector[String]].flatMap: seq =>
          locally:
            for
              planSchemaId <- seq.checked(0).flatMap(PlanSchemaId.checked)
              planKey <- seq.checked(1).flatMap(PlanKey.checked)
              noticeKey <- seq.get(2).fold(Checked(NoticeKey.empty))(NoticeKey.checked)
            yield
              planSchemaId / planKey / noticeKey
          .toDecoderResult(c.history)


object GlobalNoticeKey:

  @TestOnly
  def apply(noticeKey: String): PlannedNoticeKey =
    PlanId.Global / NoticeKey(noticeKey)

  /** Check noticeKey and return a PlannedNoticeKey in the global Plan. */
  def checked(noticeKey: String): Checked[PlannedNoticeKey] =
    PlannedNoticeKey.checked(PlanId.Global, noticeKey)
