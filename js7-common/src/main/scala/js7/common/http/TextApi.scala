package js7.common.http

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import io.circe.Json
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.web.{HttpClient, Uri}
import org.apache.pekko
import scala.concurrent.Await

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

  def executeCommand(command: String)(using IORuntime): Unit =
    val response = awaitResult(
      httpClient.post[Json, Json](uri = commandUri, command.parseJsonOrThrow))
    printer.doPrint(response.toPrettyString)

  def getApi(uri: String)(using IORuntime): Unit =
    val u = if uri == "?" then "" else uri
    val whenResponded = httpClient.get[Json](apiUri(u))
    val response = awaitResult(whenResponded)
    printer.doPrint(response)

  def requireIsResponding()(using IORuntime): Unit =
    try
      val whenResponded = httpClient.get[Json](apiUri(""))
      awaitResult(whenResponded)
      print(s"$serverName is responding")
    catch
      case ConnectionLost(t) =>
        print(s"$serverName is not responding: ${t.toStringWithCauses}")
        throw t

  def checkIsResponding()(using IORuntime): Boolean =
    try
      requireIsResponding()
      true
    catch
      case ConnectionLost(_) => false

  private def awaitResult[A](io: IO[A])(using IORuntime): A =
    try Await.result(io.unsafeToFuture(), 65.s)  // TODO Use standard Futures method await when available in subproject 'base'
    catch
      case t: Throwable =>
        t.appendCurrentStackTrace
        throw t

  protected object printer:
    def doPrint(json: Json): Unit =
      printer.doPrint(json.toPrettyString)

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
