package com.sos.jobscheduler.common.http

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.http.CirceToYaml._
import io.circe.Json
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * @author Joacim Zschimmer
  */
trait TextClient extends AkkaHttpClient with SessionApi {

  protected val print: String ⇒ Unit
  protected def serverName: String
  protected def sessionUri: String
  protected def commandUri: Uri
  protected def apiUri(tail: String): Uri

  def executeCommand(command: String): Unit = {
    val response = awaitResult(
      post[Json, Json](commandUri, yamlToJson(command).orThrow).runAsync)
    printer.doPrint(response.toYamlString)
  }

  def getApi(uri: String): Unit = {
    val u = if (uri == "?") "" else uri
    val whenResponded = get[Json](apiUri(u), 60.seconds).runAsync
    val response = awaitResult(whenResponded)
    printer.doPrint(response)
  }

  def requireIsResponding(): Unit = {
    val whenResponded = get[Json](apiUri(""), 60.seconds).runAsync
    awaitResult(whenResponded)
    print(s"$serverName is responding")
  }

  def checkIsResponding(): Boolean = {
    try {
      requireIsResponding()
      true
    } catch { case t: akka.stream.StreamTcpException ⇒
      print(s"$serverName is not responding: ${t.getMessage}")
      false
    }
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
}
