package js7.controller.web.controller.api

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.agent.client.AgentClient
import js7.base.auth.{Admission, SessionToken}
import js7.base.io.https.HttpsConfig
import js7.common.http.PekkoHttpUtils.RichPekkoUri
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.data.subagent.SubagentId
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.Uri as PekkoUri
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

object DirectorForwardRoute:

  final def forwardToDirector(
    admission: Admission,
    httpsConfig: HttpsConfig,
    forwardUri: String,
    label: String,
    subagentId: Option[SubagentId] = None)
    (using ActorSystem, IORuntime): Route =
    extractRequest: request =>
      completeIO:
        // TODO Reuse AgentClient of AgentDriver
        AgentClient.resource(admission, label = label, httpsConfig).use: agentClient =>
          val pekkoUri = admission.uri.asPekko
          var path = pekkoUri.path ?/ "agent" / "api"
          subagentId.foreach: subagentId =>
            path = path / "subagent-forward" / subagentId.string
          given IO[Option[SessionToken]] = IO(agentClient.sessionToken)
          agentClient.login() *>
            agentClient.forward(
              request,
              pekkoUri.copy(
                path = path ++ PekkoUri.Path(forwardUri),
                rawQueryString = request.uri.rawQueryString),
              dontLog = true)
