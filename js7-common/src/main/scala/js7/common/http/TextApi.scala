package js7.common.http

import io.circe.Json
import js7.base.auth.SessionToken
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.web.{HttpClient, Uri}
import js7.common.http.CirceToYaml._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Await

/**
  * @author Joacim Zschimmer
  */
trait TextApi
{
  protected val print: String => Unit
  protected def serverName: String
  protected def sessionUri: Uri
  protected def commandUri: Uri
  protected def apiUri(tail: String): Uri
  protected def httpClient: HttpClient
  protected def sessionToken: Option[SessionToken]

  implicit private def implicitSessionToken = Task(sessionToken)

  def executeCommand(command: String): Unit = {
    val response = awaitResult(
      httpClient.post[Json, Json](uri = commandUri, yamlToJson(command).orThrow))
    printer.doPrint(response.toYamlString)
  }

  def getApi(uri: String): Unit = {
    val u = if (uri == "?") "" else uri
    val whenResponded = httpClient.get[Json](apiUri(u))
    val response = awaitResult(whenResponded)
    printer.doPrint(response)
  }

  def requireIsResponding(): Unit =
    try {
      val whenResponded = httpClient.get[Json](apiUri(""))
      awaitResult(whenResponded)
      print(s"$serverName is responding")
    } catch {
      case ConnectionLost(t) =>
        print(s"$serverName is not responding: $t")
        throw t
    }

  def checkIsResponding(): Boolean =
    try {
      requireIsResponding()
      true
    } catch {
      case ConnectionLost(_) => false
    }

  private def awaitResult[A](task: Task[A]): A =
    try Await.result(task.runToFuture, 65.s)  // TODO Use standard Futures method await when available in subproject 'base'
    catch {
      case t: Throwable =>
        t.appendCurrentStackTrace
        throw t
    }

  protected object printer {
    private var needYamlDocumentSeparator = false

    def doPrint(json: Json): Unit =
      printer.doPrint(json.toYamlString)

    def doPrint(string: String): Unit = {
      if (needYamlDocumentSeparator) print("---")
      needYamlDocumentSeparator = true
      print(string.trim)
    }
  }

  object ConnectionLost {
     def apply(t: Throwable): Boolean =
       t match {
         case _: akka.stream.StreamTcpException =>
           true
         case _ if t.getMessage == "Connection was shutdown." =>  // akka.http.impl.engine.client.pool.SlotState$BusyState$$anon$1
           true
         case _ =>
           false
       }

    def unapply(t: Throwable): Option[Throwable] =
      apply(t) ? t
  }
}
