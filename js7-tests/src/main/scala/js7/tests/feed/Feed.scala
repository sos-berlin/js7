package js7.tests.feed

import cats.effect.Resource
import java.io.InputStream
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.Pekkos
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data.controller.ControllerState
import js7.data.item.ItemOperation
import js7.data.order.FreshOrder
import js7.proxy.ControllerApi
import js7.tests.feed.Feed.*
import cats.effect.IO
import fs2.Stream
import js7.base.catsutils.CatsEffectExtensions.right

final class Feed(controllerApi: ControllerApi, settings: Settings):

  def run(in: Resource[IO, InputStream]): IO[Checked[Unit]] =
    in.use(in =>
      IO {
        implicit val x = Feed.opJsonCodec
        ByteArray.fromInputStreamUnlimited(in).utf8String.parseJsonAs[Vector[Any]]
      }.flatMapT { objects =>
        val collector = objects
          .view
          .scanLeft(Collector(Nil, Nil))((collector, obj) =>
            obj match {
              case o: ItemOperation => collector.copy(itemOperations = collector.itemOperations :+ o)
              case o: FreshOrder => collector.copy(freshOrders = collector.freshOrders :+ o)
            })
          .last
        updateItems(collector.itemOperations)
          .flatMapT(_ => addOrders(collector.freshOrders))
      })

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
        .addOrders(Stream
          .iterable(orders)
          .map(_.copy(deleteWhenTerminated = true)))
        .rightAs(())


object Feed:
  def run(in: Resource[IO, InputStream], settings: Settings): IO[Checked[Unit]] =
    Pekkos.actorSystemResource("Feed")
      .flatMap(actorSystem =>
        ControllerApi.resource(
          admissionsToApiResource(settings.admissions)(actorSystem)))
      .use { controllerApi =>
        val feed = new Feed(controllerApi, settings)
        feed.run(in)
      }

  val opJsonCodec =
    import ControllerState.*

    TypedJsonCodec[Any](
      Subtype[ItemOperation],
      Subtype[FreshOrder])
      // More to come ...

  private case class Collector(itemOperations: Seq[ItemOperation], freshOrders: Seq[FreshOrder])
