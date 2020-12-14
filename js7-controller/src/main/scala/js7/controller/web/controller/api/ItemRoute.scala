package js7.controller.web.controller.api

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import akka.http.scaladsl.server.Directives.{as, complete, entity, pathEnd, post, withSizeLimit}
import akka.http.scaladsl.server.Route
import cats.syntax.flatMap._
import io.circe.Json
import js7.base.auth.{Permission, UpdateRepoPermission, ValidUserPermission}
import js7.base.crypt.SignedString
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.ScalaUtils.syntax.RichAny
import js7.base.utils.{ByteArrayToLinesObservable, FutureCompletion, SetOnce}
import js7.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import js7.common.akkautils.ByteStrings.syntax._
import js7.common.http.StreamingSupport._
import js7.common.scalautil.Logger
import js7.controller.data.ControllerState.generic.{itemPathJsonCodec, simpleItemIdJsonCodec}
import js7.controller.data.ControllerState.simpleItemJsonCodec
import js7.controller.item.{ItemsUpdater, VerifiedUpdateItems}
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.ItemRoute._
import js7.core.web.EntitySizeLimitProvider
import js7.data.crypt.VersionedItemVerifier.Verified
import js7.data.item.ItemOperation.{SimpleAddOrReplace, VersionedAddOrReplace, AddVersion, SimpleDelete, VersionedDelete}
import js7.data.item.{ItemOperation, ItemPath, SimpleItem, SimpleItemId, VersionId, VersionedItem}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny
import scala.concurrent.duration.Deadline.now

trait ItemRoute
extends ControllerRouteProvider with EntitySizeLimitProvider
{
  protected def actorSystem: ActorSystem

  protected def repoUpdater: ItemsUpdater

  private implicit def implicitScheduler: Scheduler = scheduler
  private implicit def implicitActorsystem = actorSystem

  // TODO Abort POST with error when shutting down
  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  final lazy val itemRoute: Route =
    post {
      pathEnd {
        authorizedUser(Set[Permission](ValidUserPermission, UpdateRepoPermission)) { user =>
          withSizeLimit(entitySizeLimit) (
            entity(as[HttpEntity]) { httpEntity =>
              complete {
                val startedAt = now
                var byteCount = 0L
                val versionId = SetOnce[VersionId]
                val simpleItems = Vector.newBuilder[SimpleItem]
                val deleteSimple = Vector.newBuilder[SimpleItemId]
                val versionedItems = Vector.newBuilder[Verified[VersionedItem]]
                val deleteVersioned = Vector.newBuilder[ItemPath]
                val problemOccurred = AtomicAny[Problem](null)
                httpEntity
                  .dataBytes
                  .toObservable
                  .map(_.toByteArray)
                  .pipeIf(logger.underlying.isDebugEnabled)(_.map { o => byteCount += o.length; o })
                  .flatMap(new ByteArrayToLinesObservable)
                  .mapParallelUnorderedBatch()(_
                    .parseJsonAs[ItemOperation].orThrow match {
                      case VersionedAddOrReplace(signedJson) =>
                        problemOccurred.get() match {
                          case null =>
                            val checked = verify(signedJson)
                            for (problem <- checked.left) {
                              // Delay error until input stream is completely eaten
                              problemOccurred := problem
                            }
                            checked
                          case problem =>
                            // After error, eat input stream
                            Left(problem)
                        }
                      case o => o
                    })
                  .foreachL {
                    case SimpleAddOrReplace(item) =>
                      simpleItems += item
                    case SimpleDelete(itemId) =>
                      deleteSimple += itemId
                    case Right(verifiedItem: Verified[VersionedItem] @unchecked) =>
                      versionedItems += verifiedItem
                    case VersionedDelete(path) =>
                      deleteVersioned += path
                    case Left(_) =>
                    case AddVersion(v) =>
                      versionId := v  // throws
                  }
                  .flatTap(_ => problemOccurred.get() match {
                    case null => Task.unit
                    case problem => Task.raiseError(ExitStreamException(problem))
                  })
                  .map(_ => (simpleItems.result(), deleteSimple.result(),
                    versionedItems.result(), deleteVersioned.result()))
                  .flatTap { case (simpleItems, _, versionedItems, _) =>
                    Task {
                      val d = startedAt.elapsed
                      if (d > 1.s) logger.debug(s"post controller/api/item received and verified - " +
                        itemsPerSecondString(d, simpleItems.size +
                          versionedItems.size, "items") + " · " +
                        bytesPerSecondString(d, byteCount))
                    }
                  }
                  .flatMap { case (simpleItems, deleteSimple, simpleVersioned, deleteVersioned) =>
                    val maybeVersioned = (versionId.toOption, simpleVersioned, deleteVersioned) match {
                      case (None, Seq(), Seq()) => None
                      case (Some(v), _, _) => Some(VerifiedUpdateItems.Versioned(v, simpleVersioned, deleteVersioned))
                      case _ => throw ExitStreamException(Problem.pure(s"Missing VersionId"))
                    }
                    repoUpdater
                      .updateItems(VerifiedUpdateItems(
                        VerifiedUpdateItems.Simple(simpleItems, deleteSimple),
                        maybeVersioned))
                      .map { o =>
                        if (startedAt.elapsed > 1.s) logger.debug("post controller/api/item totally: " +
                          itemsPerSecondString(startedAt.elapsed,
                            simpleItems.size + simpleItems.size, "items"))
                        o
                      }
                  }
                  .onErrorRecover { case ExitStreamException(problem) => Left(problem) }
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

  private def verify(signedString: SignedString): Checked[Verified[VersionedItem]] = {
    val verified = repoUpdater.versionedItemVerifier.verify(signedString)
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
