package js7.proxy

import cats.data.EitherT
import cats.effect.{IO, ResourceIO}
import io.circe.Json
import js7.base.circeutils.CirceUtils.{RichCirceEither, RichJson}
import js7.base.problem.Checked
import js7.base.web.HttpClient
import js7.controller.client.HttpControllerApi

trait ControllerApiWithHttp:
  protected def apiResource(implicit src: sourcecode.Enclosing)
  : ResourceIO[HttpControllerApi]

  def httpPostJson(uriTail: String, jsonString: String): IO[Checked[String]] =
    (for
      requestJson <- EitherT(IO(io.circe.parser.parse(jsonString).toChecked))
      responseJson <- EitherT(
        apiResource.use(api =>
          HttpClient.liftProblem(
            api.post[Json, Json](uriTail, requestJson))))
    yield responseJson).value
      .map(_.map(_.compactPrint))

  /** HTTP GET
    * @param uriTail path and query of the URI
    * @return `Either.Left(Problem)` or `Either.Right(json: String)`
    */
  def httpGetJson(uriTail: String): IO[Checked[String]] =
    apiResource.use(api =>
      HttpClient.liftProblem(
        api.get[Json](uriTail)
      ).map(_.map(_.compactPrint)))
