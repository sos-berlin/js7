package js7.data.board

import io.circe.{Codec, Decoder}
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.typed.Subtype
import js7.base.problem.Checked
import js7.base.utils.Big
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.NoticePlace.*
import js7.data.order.OrderId
import js7.data.plan.PlanId

/** A NoticePlace may contain both a notice and an expectation,
  * when an order awaits notices from multiple Boards.
  *
  * @param isAnnounced only for PlannedBoard
  */
final case class NoticePlace(
  notice: Option[Notice] = None,
  expectingOrderIds: Set[OrderId] = Set.empty,
  isAnnounced: Boolean = false,
  isInConsumption: Boolean = false,
  consumptionCount: Int = 0)
extends Big:

  override def toString =
    s"NoticePlace(${notice getOrElse ""
    }${isAnnounced ?? " isAnnounced"
    }${isInConsumption ?? " isInConsumption"
    }${(consumptionCount != 0) ?? s" consumptionCount=$consumptionCount"
    }${expectingOrderIds.nonEmpty ?? s" expectingOrderIds=${expectingOrderIds.toVector.sorted.mkString(" ")}"})"

  def checked: Checked[this.type] =
    Right(this)

  def isEmpty: Boolean =
    notice.isEmpty &&
      expectingOrderIds.isEmpty &&
      !isAnnounced &&
      !isInConsumption &&
      consumptionCount == 0

  inline def nonEmpty: Boolean =
    !isEmpty

  def isInUse: Boolean =
    expectingOrderIds.nonEmpty || isInConsumption || consumptionCount != 0

  def estimatedSnapshotSize: Int =
    (isAnnounced || isInConsumption || consumptionCount != 0).toInt

  def toSnapshotStream(noticeId: NoticeId): fs2.Stream[fs2.Pure, Snapshot | Notice] =
    fs2.Stream.fromOption(notice) ++
      (isAnnounced || isInConsumption || consumptionCount != 0).thenStream:
        Snapshot(noticeId, isAnnounced, isInConsumption, consumptionCount)

  def recoverSnapshot(snapshot: Notice | NoticePlace.Snapshot): NoticePlace =
    snapshot match
      case snapshot: Notice =>
        copy(
          notice = Some(snapshot))
      case snapshot: NoticePlace.Snapshot =>
        copy(
          isAnnounced = snapshot.isAnnounced,
          isInConsumption = snapshot.isInConsumption,
          consumptionCount = snapshot.consumptionCount)

  def announce: NoticePlace =
    copy(isAnnounced = true)

  def post(notice: Notice): NoticePlace =
    copy(
      notice = Some(notice),
      isAnnounced = false,
      isInConsumption = false)

  def removeNotice: NoticePlace =
    copy(notice = None)

  def addExpecting(orderId: OrderId): NoticePlace =
    copy(expectingOrderIds = expectingOrderIds + orderId)

  def removeExpecting(orderId: OrderId): NoticePlace =
    copy(expectingOrderIds = expectingOrderIds - orderId)

  def startConsumption(orderId: OrderId): NoticePlace =
    copy(
      expectingOrderIds = expectingOrderIds - orderId,
      isInConsumption = true,
      consumptionCount = consumptionCount + 1)

  def finishConsumption(succeeded: Boolean): NoticePlace =
    val isLast = consumptionCount == 1
    copy(
      notice =
        if succeeded && isInConsumption && isLast then
          None
        else
          notice,
      //post already did this: isAnnounced = isAnnounced && !isLast,
      isInConsumption = isInConsumption && !isLast,
      consumptionCount = consumptionCount - 1)


object NoticePlace:

  val empty: NoticePlace =
    NoticePlace()

  final case class Snapshot(
    noticeId: NoticeId,
    isAnnounced: Boolean = false,
    isInConsumption: Boolean = false,
    consumptionCount: Int = 0)
  extends NoticeSnapshot:
    override def productPrefix = "NoticePlace.Snapshot"

    export noticeId.plannedNoticeKey

    def plannedBoardId: PlannedBoardId =
      noticeId.plannedBoardId

    def boardPath: BoardPath =
      noticeId.boardPath

  object Snapshot:
    private val jsonCodec = deriveCodecWithDefaults[Snapshot]

    private val jsonDecoder: Decoder[Snapshot] = c =>
      c.value.asObject.fold(jsonCodec(c)): obj =>
        if obj("boardPath").isDefined then
          // COMPATIBLE with v2.7.3
          for
            boardPath <- c.get[BoardPath]("boardPath")
            noticeKey <- c.get[NoticeKey]("noticeId")
            isAnnounced <- c.getOrElse[Boolean]("isAnnounced")(false)
            isInConsumption <- c.getOrElse[Boolean]("isInConsumption")(false)
            consumptionCount <- c.getOrElse[Int]("consumptionCount")(0)
          yield
            NoticePlace.Snapshot(
              PlanId.Global / boardPath / noticeKey,
              isAnnounced = isAnnounced,
              isInConsumption = isInConsumption,
              consumptionCount)
        else
          jsonCodec(c)

    val subtype: Subtype[Snapshot] =
      Subtype.named(Codec.AsObject.from(jsonDecoder, jsonCodec), "NoticePlace")
