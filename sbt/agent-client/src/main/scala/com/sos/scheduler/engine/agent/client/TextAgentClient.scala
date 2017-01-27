package com.sos.scheduler.engine.agent.client

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.client.TextAgentClient._
import com.sos.scheduler.engine.agent.data.web.AgentUris
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.{UserAndPassword, UserId}
import com.sos.scheduler.engine.common.configutils.Configs
import com.sos.scheduler.engine.common.scalautil.Futures.awaitResult
import com.sos.scheduler.engine.common.sprayutils.YamlJsonConversion.yamlToJsValue
import com.sos.scheduler.engine.common.sprayutils.https.{Https, KeystoreReference}
import com.sos.scheduler.engine.common.sprayutils.sprayclient.ExtendedPipelining.extendedSendReceive
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.data.agent.AgentAddress
import java.nio.charset.StandardCharsets._
import scala.concurrent.Future
import scala.util.Try
import spray.client.pipelining._
import spray.http.CacheDirectives.{`no-cache`, `no-store`}
import spray.http.HttpHeaders.{Accept, `Cache-Control`}
import spray.http.MediaTypes._
import spray.http.{BasicHttpCredentials, HttpEntity}
import spray.httpx.encoding.Gzip
import spray.httpx.marshalling.Marshaller
import spray.json.{JsValue, JsonParser}

/**
  * @author Joacim Zschimmer
  */
private[agent] final class TextAgentClient(agentUri: AgentAddress, print: String ⇒ Unit,
  userAndPassword: Option[UserAndPassword] = None, keystore: Option[KeystoreReference] = None)
extends AutoCloseable {

  private val agentUris = AgentUris(agentUri)
  private implicit val actorSystem = ActorSystem("AgentClient", Configs.loadResource(ConfigurationResource))
  import actorSystem.dispatcher

  private val hostConnectorSetupOption = keystore/*Option*/ map { o ⇒ Https.toHostConnectorSetup(o, uri = agentUri.string) }
  private val pipeline = {
    val addUserAndPassword: RequestTransformer = userAndPassword match {
      case Some(UserAndPassword(UserId(user), SecretString(password))) ⇒ addCredentials(BasicHttpCredentials(user, password))
      case _ ⇒ identity
    }
    addUserAndPassword ~>
      addHeader(Accept(`text/plain`)) ~>
      addHeader(`Cache-Control`(`no-cache`, `no-store`)) ~>
      encode(Gzip) ~>
      extendedSendReceive(60.s.toFiniteDuration, hostConnectorSetupOption) ~>
      decode(Gzip) ~>
      unmarshal[String]
  }
  private var needYamlDocumentSeparator = false

  def close() = actorSystem.shutdown()

  def executeCommand(command: String): Unit =
    doPrint(resultString(pipeline(Post(agentUris.command, forceToJson(command)))))

  def get(uri: String): Unit =
    doPrint(resultString(pipeline(Get(agentUris.api(uri)))))

  def checkIsResponding(): Boolean = {
    try {
      requireIsResponding()
      true
    }
    catch {
      case t: spray.can.Http.ConnectionAttemptFailedException ⇒
        print(s"JobScheduler Agent is not responding: ${t.getMessage}")
        false
    }
  }

  def requireIsResponding(): Unit = {
    resultString(pipeline(Get(agentUris.overview)))
    print("JobScheduler Agent is responding")
  }

  private def doPrint(string: String): Unit = {
    if (needYamlDocumentSeparator) print("---")
    needYamlDocumentSeparator = true
    print(string.trim)
  }
}

object TextAgentClient {
  private val ConfigurationResource = JavaResource("com/sos/scheduler/engine/agent/client/main/akka.conf")

  implicit val JsValueMarshaller = Marshaller.of[JsValue](`application/json`) { (value, contentType, ctx) ⇒
    ctx.marshalTo(HttpEntity(`application/json`, value.compactPrint.getBytes(UTF_8)))
  }

  private def forceToJson(jsonOrYaml: String): JsValue =
    Try { JsonParser(jsonOrYaml) } getOrElse yamlToJsValue(jsonOrYaml)

  private def resultString(future: Future[String]) = awaitResult(future, 2 * AgentClient.RequestTimeout)
}
