package js7.common.http

import cats.effect.IO
import io.circe.Json
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.{HttpClient, Uri}
import org.apache.pekko

/**
  * @author Joacim Zschimmer
  */
trait TextApi:
  protected val print: String => Unit
  protected def serverName: String
  protected def sessionUri: Uri
  protected def commandUri: Uri
  protected def apiUri(tail: String): Uri
  protected def httpClient: HttpClient
  protected def sessionToken: Option[SessionToken]

  implicit private def implicitSessionToken: IO[Option[SessionToken]] = IO(sessionToken)

  def executeCommand(command: String): IO[Unit] =
    httpClient.post[Json, Json](uri = commandUri, command.parseJsonOrThrow)
      .flatMap: response =>
        IO(printer.doPrint(response.compactPrint))

  def getApi(uri: String): IO[Unit] =
    val u = if uri == "?" then "" else uri
    httpClient.get[Json](apiUri(u)).flatMap: response =>
      IO(printer.doPrint(response))

  def requireIsResponding: IO[Unit] =
    httpClient.get[Json](apiUri("")).flatMap: _ =>
      IO(printer.doPrint(s"$serverName is responding"))
    .onError:
      case ConnectionLost(t) =>
        IO(print(s"$serverName is not responding: ${t.toStringWithCauses}"))

  def checkIsResponding: IO[Boolean] =
    requireIsResponding.as(true)
      .recoverWith:
        case ConnectionLost(_) => IO.pure(false)


  protected object printer:
    def doPrint(json: Json): Unit =
      printer.doPrint(json.compactPrint)

    def doPrint(string: String): Unit =
      print(string.trim)

  object ConnectionLost:
    def apply(t: Throwable): Boolean =
      t match
        case _: pekko.stream.StreamTcpException =>
          true
        case t: RuntimeException =>
          t.toString.contains("java.net.ConnectException: Connection refused")
        case _ if t.getMessage == "Connection was shutdown." =>  // pekko.http.impl.engine.client.pool.SlotState$BusyState$$anon$1
          true
        case _ =>
          false

    def unapply(t: Throwable): Option[Throwable] =
      apply(t) ? t
