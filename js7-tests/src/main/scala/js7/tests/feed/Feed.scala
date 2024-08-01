package js7.tests.feed

import cats.effect.kernel.Deferred
import cats.effect.{IO, ResourceIO}
import fs2.Stream
import java.io.InputStream
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.data.ByteArray
import js7.base.fs2utils.StreamExtensions.*
import js7.base.log
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.base.utils.Lazy
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.Pekkos
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data.controller.ControllerState
import js7.data.controller.ControllerState.signableItemJsonCodec
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.item.ItemOperation.{AddOrChangeOperation, AddOrChangeSigned, AddOrChangeSimple, AddVersion}
import js7.data.item.{InventoryItem, ItemOperation, ItemSigner, SignableItem, UnsignedSimpleItem, VersionId, VersionedItem}
import js7.data.order.OrderEvent.OrderTerminated
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.proxy.ControllerApi

final class Feed(controllerApi: ControllerApi, conf: FeedConf):

  private lazy val itemSigner: Option[ItemSigner[SignableItem]] =
    conf.documentSigner.map:
      ItemSigner(_, ControllerState.signableItemJsonCodec)

  def run(in: ResourceIO[InputStream]): IO[Unit] =
    readObjects(in).flatMap: objects =>
      val collector = collect(objects)
      Deferred[IO, Unit].flatMap: loggingStarted =>
        IO.both(
          logEvents(collector, loggingStarted),
          loggingStarted.get *>
            updateItems(collector.allItemOperations)
              .flatMapT: _ =>
                addOrders(collector.freshOrders)
              .map(_.orThrow))
      .void

  private def readObjects(in: ResourceIO[InputStream]): IO[Vector[Any]] =
    in.use: in =>
      IO:
        given TypedJsonCodec[Any] = Feed.opJsonCodec
        ByteArray.fromInputStreamUnlimited(in).utf8String.parseJsonAs[Vector[Any]]
          .orThrow

  private def collect(objects: Vector[Any]): Collector =
    objects
      .view
      .scanLeft(Collector()): (collector, obj) =>
        obj match
          case o: InventoryItem => collector.copy(items = collector.items :+ o)
          case o: ItemOperation => collector.copy(itemOperations = collector.itemOperations :+ o)
          case o: FreshOrder => collector.copy(freshOrders = collector.freshOrders :+ o)
      .last

  private def updateItems(ops: Seq[ItemOperation]): IO[Checked[Unit]] =
    if ops.isEmpty then
      IO.right(())
    else
      controllerApi
        .updateItems(Stream.iterable(ops))
        .rightAs(())

  private def addOrders(orders: Seq[FreshOrder]): IO[Checked[Unit]] =
    if orders.isEmpty then
      IO.right(())
    else
      controllerApi
        .addOrders:
          Stream.iterable(orders).map(_.copy(deleteWhenTerminated = true))
        .rightAs(())

  private def logEvents(collector: Collector, started: Deferred[IO, Unit]): IO[Unit] =
    if conf.watchOrders then
      val orderIds = collector.freshOrders.map(_.id)
      logLine(s"Watching ${orderIds.mkString(", ")} ...") *>
        logOrderEvents(orderIds.toSet, started)
    else
      started.complete(()).void

  private def logOrderEvents(orderIds: Set[OrderId], started: Deferred[IO, Unit]): IO[Unit] =
    controllerApi.eventAndStateStream()
      .onFirst: _ =>
        // Stream starts with a dummy ProxyStarted event (after ControllerState has been read)
        started.complete(()).void
      .map(_.stampedEvent)
      .collect:
        case Stamped(_, milli, KeyedEvent(orderId: OrderId, event: OrderEvent))
          if orderIds(orderId) =>
          (milli, orderId, event)
      .evalTap((milli, orderId, event) =>
        logLine(s"${Timestamp.ofEpochMilli(milli)} $orderId <-: $event"))
      .collect:
        case (_, orderId, _: OrderTerminated) => orderId
      .scan(orderIds): (remaining, orderId) =>
        remaining - orderId
      .takeWhile(_.nonEmpty)
      .compile
      .drain

  private def logLine(line: String): IO[Unit] =
    IO(println(line))

  private case class Collector(
    itemOperations: Seq[ItemOperation] = Nil,
    items: Seq[InventoryItem] = Nil,
    freshOrders: Seq[FreshOrder] = Nil):

    def allItemOperations: Seq[ItemOperation] =
      itemOperations ++ itemsToOperations

    private def itemsToOperations: Seq[ItemOperation] =
      val versionId = Lazy(VersionId(Timestamp.now.toIsoString))
      val operations: Seq[AddOrChangeOperation] =
        // Execute this eagerly to fill versionId
        items
          .map:
            case item: VersionedItem if item.id.versionId.isAnonymous =>
              item.withVersion(versionId.value)
            case o => o
          .map:
            case item: UnsignedSimpleItem => AddOrChangeSimple(item)
            case item: SignableItem =>
              val signer = itemSigner.getOrElse:
                throw new IllegalArgumentException("Missing PGP private key when adding items")
              AddOrChangeSigned(signer.sign(item).signedString)

      versionId.toOption.map(AddVersion(_)) ++: operations


object Feed:

  private val logger = Logger[this.type]

  def run(in: ResourceIO[InputStream], conf: FeedConf): IO[Unit] =
    Pekkos.actorSystemResource("Feed")
      .flatMap: actorSystem =>
        ControllerApi.resource:
          admissionsToApiResource(conf.admissions)(actorSystem)
      .use: controllerApi =>
        Feed(controllerApi, conf).run(in)

  private[feed] val opJsonCodec: TypedJsonCodec[Any] =
    import ControllerState.*

    TypedJsonCodec[Any](
      Subtype[ItemOperation],
      Subtype[FreshOrder])
    | ControllerState.inventoryItemJsonCodec
