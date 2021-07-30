package js7.controller.web.controller.api

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import akka.http.scaladsl.server.Directives.{as, complete, entity, pathEnd, post, withSizeLimit}
import akka.http.scaladsl.server.Route
import io.circe.Json
import js7.base.auth.{Permission, UpdateItemPermission, ValidUserPermission}
import js7.base.crypt.SignedString
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichEitherF}
import js7.base.utils.{ByteSequenceToLinesObservable, FutureCompletion}
import js7.common.akkahttp.CirceJsonSupport.jsonMarshaller
import js7.common.akkautils.ByteStrings.syntax._
import js7.common.http.StreamingSupport._
import js7.controller.item.ItemUpdater
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.ItemRoute._
import js7.core.web.EntitySizeLimitProvider
import js7.data.controller.ControllerState._
import js7.data.controller.VerifiedUpdateItems
import js7.data.crypt.SignedItemVerifier.Verified
import js7.data.item.{ItemOperation, SignableItem}
import monix.execution.Scheduler
import scala.concurrent.duration.Deadline.now

trait ItemRoute
extends ControllerRouteProvider with EntitySizeLimitProvider
{
  protected def actorSystem: ActorSystem

  protected def itemUpdater: ItemUpdater

  private implicit def implicitScheduler: Scheduler = scheduler
  private implicit def implicitActorsystem = actorSystem

  // TODO Abort POST with error when shutting down
  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  final lazy val itemRoute: Route =
    post {
      pathEnd {
        authorizedUser(Set[Permission](ValidUserPermission, UpdateItemPermission)) { user =>
          withSizeLimit(entitySizeLimit) (
            entity(as[HttpEntity]) { httpEntity =>
              complete {
                val startedAt = now
                var byteCount = 0L
                val operations = httpEntity
                  .dataBytes
                  .toObservable
                  .map(_.toByteArray)
                  .pipeIf(logger.underlying.isDebugEnabled)(_.map { o => byteCount += o.length; o })
                  .flatMap(new ByteSequenceToLinesObservable)
                  .mapParallelUnorderedBatch()(_
                    .parseJsonAs[ItemOperation].orThrow)
                VerifiedUpdateItems.fromOperations(operations, verify, user)
                  .flatMapT { verifiedUpdateItems =>
                    val itemCount = verifiedUpdateItems.itemCount
                    val d = startedAt.elapsed
                    if (d > 1.s) logger.debug(s"post controller/api/item received and verified - " +
                      itemsPerSecondString(d, itemCount, "items") + " · " +
                      bytesPerSecondString(d, byteCount))

                    itemUpdater
                      .updateItems(verifiedUpdateItems)
                      .map { o =>
                        if (startedAt.elapsed > 1.s) logger.debug("post controller/api/item totally: " +
                          itemsPerSecondString(startedAt.elapsed, itemCount, "items"))
                        o
                      }
                  }
                  .map[ToResponseMarshallable] {
                    case Left(problem) =>
                      logger.debug(problem.toString)
                      BadRequest -> problem
                    case Right(Completed) =>
                      OK -> emptyJsonObject
                  }
                  .runToFuture
              }
            })
        }
      }
    }

  private def verify(signedString: SignedString): Checked[Verified[SignableItem]] = {
    val verified = itemUpdater.signedItemVerifier.verify(signedString)
    verified match {
      case Left(problem) => logger.warn(problem.toString)
      case Right(verified) => logger.info(Logger.SignatureVerified, verified.toString)
    }
    verified
  }
}

object ItemRoute
{
  private val logger = Logger(getClass)
  private val emptyJsonObject = Json.obj()

  private case class ExitStreamException(problem: Problem) extends Exception
}
