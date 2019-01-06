package com.sos.jobscheduler.common.http

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.http.CirceToYaml._
import io.circe.Json
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * @author Joacim Zschimmer
  */
trait TextApi {

  protected val print: String ⇒ Unit
  protected def serverName: String
  protected def sessionUri: String
  protected def commandUri: Uri
  protected def apiUri(tail: String): Uri
  protected def httpClient: AkkaHttpClient

  def executeCommand(command: String): Unit = {
    val response = awaitResult(
      httpClient.post[Json, Json](uri = commandUri.toString, yamlToJson(command).orThrow).runToFuture)
    printer.doPrint(response.toYamlString)
  }

  def getApi(uri: String): Unit = {
    val u = if (uri == "?") "" else uri
    val whenResponded = httpClient.get[Json](apiUri(u), 60.seconds).runToFuture
    val response = awaitResult(whenResponded)
    printer.doPrint(response)
  }

  def requireIsResponding(): Unit =
    try {
      val whenResponded = httpClient.get[Json](apiUri(""), 60.seconds).runToFuture
      awaitResult(whenResponded)
      print(s"$serverName is responding")
    } catch {
      case ConnectionLost(t) ⇒
        print(s"$serverName is not responding: $t")
        throw t
    }

  def checkIsResponding(): Boolean =
    try {
      requireIsResponding()
      true
    } catch {
      case ConnectionLost(_) ⇒ false
    }

  private def awaitResult[A](future: Future[A]): A =
    try Await.result(future, 65.seconds)  // TODO Use standard Futures method await when available in subproject 'base'
    catch {
      case t: Throwable ⇒
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
         case _: akka.stream.StreamTcpException ⇒
           true
         case _ if t.getMessage == "Connection was shutdown." ⇒  // akka.http.impl.engine.client.pool.SlotState$BusyState$$anon$1
           true
       }

    def unapply(t: Throwable): Option[Throwable] =
      if (apply(t)) Some(t) else None
  }
}
