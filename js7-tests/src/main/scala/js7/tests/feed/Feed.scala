package js7.tests.feed

import cats.effect.Resource
import java.io.InputStream
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.data.ByteArray
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkautils.Akkas
import js7.controller.client.AkkaHttpControllerApi.admissionToApiResource
import js7.data.controller.ControllerState
import js7.data.item.ItemOperation
import js7.proxy.ControllerApi
import monix.eval.Task
import monix.reactive.Observable

final class Feed(controllerApi: ControllerApi, settings: Settings)
{
  def run(in: Resource[Task, InputStream]): Task[Checked[Completed]] =
    in.use(in =>
      Task {
        implicit val x = Feed.opJsonCodec
        ByteArray.fromInputStreamUnlimited(in).utf8String.parseJsonAs[Vector[Any]]
      }.flatMapT(ops =>
        controllerApi.updateItems(Observable
          .fromIterable(ops)
          .map(_.asInstanceOf[ItemOperation]))))
}

object Feed
{
  def run(in: Resource[Task, InputStream], settings: Settings): Task[Checked[Completed]] =
    Akkas.actorSystemResource("Feed")
      .flatMap(actorSystem =>
        ControllerApi.resource(
          settings.admissions.map(admissionToApiResource(_)(actorSystem))))
      .use { controllerApi =>
        val feed = new Feed(controllerApi, settings)
        feed.run(in)
      }

  val opJsonCodec = {
    import ControllerState._

    TypedJsonCodec[Any](
      Subtype[ItemOperation])
      // More to come ...
  }
}
