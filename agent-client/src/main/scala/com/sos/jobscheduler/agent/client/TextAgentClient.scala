package com.sos.jobscheduler.agent.client

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.coding.Gzip
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.headers.CacheDirectives.{`no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{Accept, Authorization, BasicHttpCredentials, `Cache-Control`}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, StatusCode}
import akka.stream.ActorMaterializer
import com.sos.jobscheduler.agent.client.TextAgentClient._
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.AkkaHttpClientUtils.RichHttpResponse
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.decodeResponse
import com.sos.jobscheduler.common.akkahttp.CirceToYaml.yamlToJson
import com.sos.jobscheduler.common.akkahttp.https.{Https, KeystoreReference}
import com.sos.jobscheduler.common.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.data.agent.AgentAddress
import io.circe.Json
import scala.concurrent.Future
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
private[agent] final class TextAgentClient(agentUri: AgentAddress, print: String ⇒ Unit,
  userAndPassword: Option[UserAndPassword] = None, keystore: Option[KeystoreReference] = None)
extends AutoCloseable {

  private val agentUris = AgentUris(agentUri)
  private val actorSystem = ActorSystem("TextAgentClient", Configs.loadResource(ConfigurationResource))
  import actorSystem.dispatcher
  private implicit val materialized = ActorMaterializer()(actorSystem)
  private val http = Http(actorSystem)

  private val httpsConnectionContext = keystore/*Option*/ map Https.toHttpsConnectionContext getOrElse http.defaultClientHttpsContext
  private var needYamlDocumentSeparator = false

  def close() = actorSystem.terminate()

  def executeCommand(command: String): Unit =
    doPrint(singleRequest(HttpRequest(POST, agentUris.command, entity = HttpEntity(`application/json`, forceToJson(command).compactPrint))) await 99.s)

  def get(uri: String): Unit =
    doPrint(singleRequest(HttpRequest(GET, agentUris.api(uri))) await 99.s)

  def checkIsResponding(): Boolean = {
    try {
      requireIsResponding()
      true
    } catch { case t: akka.stream.StreamTcpException ⇒
      print(s"JobScheduler Agent is not responding: ${t.getMessage}")
      false
    }
  }

  def requireIsResponding(): Unit = {
    singleRequest(HttpRequest(GET, agentUris.overview)) await 99.s
    print("JobScheduler Agent is responding")
  }

  private def singleRequest(request: HttpRequest): Future[String] = {
    val authentication = userAndPassword map {
      case UserAndPassword(UserId(user), SecretString(password)) ⇒ Authorization(BasicHttpCredentials(user, password))
    }
    val myRequest = request.withHeaders(Vector(Accept(`text/plain`), `Cache-Control`(`no-cache`, `no-store`)) ++ authentication ++ request.headers)
    for {
      httpResponse ← http.singleRequest(Gzip.encodeMessage(myRequest), httpsConnectionContext)
      string ←
        if (httpResponse.status.isSuccess)
          decodeResponse(httpResponse).utf8StringFuture
        else
          for (message ← decodeResponse(httpResponse).utf8StringFuture) yield
            throw new HttpException(httpResponse.status, message)
    } yield string
  }

  private def doPrint(string: String): Unit = {
    if (needYamlDocumentSeparator) print("---")
    needYamlDocumentSeparator = true
    print(string.trim)
  }
}

object TextAgentClient {
  private val ConfigurationResource = JavaResource("com/sos/jobscheduler/agent/client/main/akka.conf")

  private def forceToJson(jsonOrYaml: String): Json =
    Try { jsonOrYaml.parseJson } getOrElse yamlToJson(jsonOrYaml)

  final class HttpException(val status: StatusCode, message: String) extends RuntimeException(s"$status: $message".trim)
}
