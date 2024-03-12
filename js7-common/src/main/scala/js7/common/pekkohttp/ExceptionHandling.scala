package js7.common.pekkohttp

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.unsafe.IORuntime
import cats.implicits.catsSyntaxEitherId
import com.typesafe.config.Config
import js7.base.log.Logger
import js7.base.problem.{Problem, ProblemException}
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkohttp.ExceptionHandling.*
import js7.common.pekkohttp.StandardDirectives.ioRoute
import js7.common.pekkohttp.StandardMarshallers.*
import org.apache.pekko
import org.apache.pekko.http.scaladsl.model.StatusCodes.{InternalServerError, ServiceUnavailable}
import org.apache.pekko.http.scaladsl.model.{HttpRequest, StatusCode}
import org.apache.pekko.http.scaladsl.server.Directives.{complete, extractRequest}
import org.apache.pekko.http.scaladsl.server.{ExceptionHandler, Route}
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
trait ExceptionHandling:

  protected def whenShuttingDown: Deferred[IO, Deadline]
  protected def config: Config
  protected def ioRuntime: IORuntime

  private given IORuntime = ioRuntime

  private lazy val respondWithException = config.getBoolean("js7.web.server.verbose-error-messages")

  protected final lazy val isShuttingDown: IO[Option[Deadline]] =
    whenShuttingDown.get
      .map(Some(_))
      .timeoutTo(ZeroDuration, IO.none) // TODO Is this reliable? Maybe use SignallingRef

  protected final lazy val shutdownSignaled: IO[Unit] =
    whenShuttingDown.get.as(().asRight[Throwable])

  implicit protected final lazy val exceptionHandler: ExceptionHandler =
    ExceptionHandler:
      case e: HttpStatusCodeException =>
        complete(e.statusCode -> e.problem)

      case e: pekko.pattern.AskTimeoutException =>
        ioRoute:
          isShuttingDown
            .map(Some(_))
            .timeoutTo(0.s, IO.none)
            .map:
              case None =>
                completeWithError(InternalServerError, e)
              case Some(_) =>
                extractRequest: request =>
                  webLogger.debug(toLogMessage(request, e), e.nullIfNoStackTrace)
                  complete(ServiceUnavailable -> Problem.pure("Shutting down"))

      case e: ProblemException =>
        // TODO Better use Checked instead of ProblemException
        extractRequest { request =>
          for t <- e.ifStackTrace do webLogger.debug(toLogMessage(request, e), t)
          complete(e.problem.httpStatusCode -> e.problem)
        }

      case e =>
        completeWithError(InternalServerError, e)

  private def completeWithError(status: StatusCode, e: Throwable): Route =
    extractRequest { request =>
      def msg = toLogMessage(request, e)
      //if (whenShuttingDown.isCompleted) {
        webLogger.debug(msg, e)
      //} else {
      //  webLogger.warn(msg, e.nullIfNoStackTrace)
      //}
      if respondWithException then
        complete(status -> Problem.fromThrowable(e))
      else
        complete(status)
    }

  protected final def seal(route: Route): Route =
    Route.seal(route)(exceptionHandler = exceptionHandler)


object ExceptionHandling:
  val webLogger = Logger("js7.web.exception")

  private def toLogMessage(request: HttpRequest, throwable: Throwable) =
    s"Error while handling ${request.method.value} ${request.uri}: ${throwable.toStringWithCauses}"
