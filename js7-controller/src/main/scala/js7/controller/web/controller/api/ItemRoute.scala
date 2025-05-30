package js7.controller.web.controller.api

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import io.circe.Json
import js7.base.auth.{Permission, UpdateItemPermission, ValidUserPermission}
import js7.base.crypt.SignedString
import js7.base.fs2utils.StreamExtensions.mapParallelBatch
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.LineSplitterPipe
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichEitherF}
import js7.common.http.StreamingSupport.*
import js7.common.pekkohttp.CirceJsonSupport.jsonMarshaller
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.controller.item.ItemUpdater
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.ItemRoute.*
import js7.core.web.EntitySizeLimitProvider
import js7.data.controller.ControllerState.*
import js7.data.controller.VerifiedUpdateItems
import js7.data.crypt.SignedItemVerifier.Verified
import js7.data.item.{ItemOperation, SignableItem}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.marshalling.ToResponseMarshallable
import org.apache.pekko.http.scaladsl.model.HttpEntity
import org.apache.pekko.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import org.apache.pekko.http.scaladsl.server.Directives.{as, entity, pathEnd, post, withSizeLimit}
import org.apache.pekko.http.scaladsl.server.Route
import scala.concurrent.duration.Deadline.now

trait ItemRoute
extends ControllerRouteProvider, EntitySizeLimitProvider:

  protected def actorSystem: ActorSystem

  protected def itemUpdater: ItemUpdater

  private given IORuntime = ioRuntime
  private given ActorSystem = actorSystem

  final lazy val itemRoute: Route =
    (post & pathEnd):
      authorizedUser(Set[Permission](ValidUserPermission, UpdateItemPermission)): user =>
        (withSizeLimit(entitySizeLimit) & entity(as[HttpEntity])): httpEntity =>
          completeIO(IO.defer:
            val startedAt = now
            var byteCount = 0L
            val operations = httpEntity
              .dataBytes
              .asFs2Stream(bufferSize = prefetch)
              .pipeIf(logger.underlying.isDebugEnabled)(_.map { o => byteCount += o.length; o })
              .through(LineSplitterPipe())
              .mapParallelBatch():
                _.parseJsonAs[ItemOperation].orThrow
            VerifiedUpdateItems.fromOperations(operations, verify, user)
              .flatMapT: verifiedUpdateItems =>
                IO.defer:
                  val itemCount = verifiedUpdateItems.itemCount
                  val d = startedAt.elapsed
                  if d > 1.s then
                    logger.debug("POST controller/api/item received and verified - " +
                      itemsPerSecondString(d, itemCount, "items") + " · " +
                      bytesPerSecondString(d, byteCount))

                  itemUpdater
                    .updateItems(verifiedUpdateItems)
                    .flatTap(o => IO:
                      if startedAt.elapsed > 1.s then
                        logger.debug("POST controller/api/item totally: " +
                          itemsPerSecondString(startedAt.elapsed, itemCount, "items")))
              .map[ToResponseMarshallable]:
                case Left(problem) =>
                  logger.warn(problem.toString)
                  BadRequest -> problem
                case Right(Completed) =>
                  OK -> emptyJsonObject)

  private def verify(signedString: SignedString): Checked[Verified[SignableItem]] =
    val verified = itemUpdater.signedItemVerifier.verify(signedString)
    for verified <- verified do logger.info(Logger.SignatureVerified, verified.toString)
    verified


object ItemRoute:
  private val logger = Logger[this.type]
  private val emptyJsonObject = Json.obj()

  private case class ExitStreamException(problem: Problem) extends Exception
