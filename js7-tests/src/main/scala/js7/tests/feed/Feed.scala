package js7.tests.feed

import cats.effect.Resource
import java.io.InputStream
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.akkautils.Akkas
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResource
import js7.data.controller.ControllerState
import js7.data.item.ItemOperation
import js7.data.order.FreshOrder
import js7.proxy.ControllerApi
import js7.tests.feed.Feed.*
import monix.eval.Task
import monix.reactive.Observable

final class Feed(controllerApi: ControllerApi, settings: Settings)
{
  def run(in: Resource[Task, InputStream]): Task[Checked[Unit]] =
    in.use(in =>
      Task {
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

  private def updateItems(ops: Seq[ItemOperation]): Task[Checked[Unit]] =
    if ops.isEmpty then
      Task.right(())
    else
      controllerApi
        .updateItems(Observable.fromIterable(ops))
        .rightAs(())

  private def addOrders(orders: Seq[FreshOrder]): Task[Checked[Unit]] =
    if orders.isEmpty then
      Task.right(())
    else
      controllerApi
        .addOrders(Observable
          .fromIterable(orders)
          .map(_.copy(deleteWhenTerminated = true)))
        .rightAs(())
}

object Feed
{
  def run(in: Resource[Task, InputStream], settings: Settings): Task[Checked[Unit]] =
    Akkas.actorSystemResource("Feed")
      .flatMap(actorSystem =>
        ControllerApi.resource(
          admissionsToApiResource(settings.admissions)(actorSystem)))
      .use { controllerApi =>
        val feed = new Feed(controllerApi, settings)
        feed.run(in)
      }

  val opJsonCodec = {
    import ControllerState.*

    TypedJsonCodec[Any](
      Subtype[ItemOperation],
      Subtype[FreshOrder])
      // More to come ...
  }

  private case class Collector(itemOperations: Seq[ItemOperation], freshOrders: Seq[FreshOrder])
}
