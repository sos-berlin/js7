package js7.agent.web

import io.circe.Codec
import js7.agent.data.commands.AgentCommand
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.pekkohttp.PekkoHttpServerUtils.completeTask
import js7.common.pekkohttp.StandardMarshallers.*
import js7.core.command.CommandMeta
import js7.core.web.EntitySizeLimitProvider
import js7.data.command.{CommandHandlerDetailed, CommandHandlerOverview, CommandRunOverview}
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import org.apache.pekko.http.scaladsl.model.headers.CacheDirectives.`max-age`
import org.apache.pekko.http.scaladsl.model.headers.`Cache-Control`
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

/**
 * @author Joacim Zschimmer
 */
trait CommandWebService
extends AgentRouteProvider with EntitySizeLimitProvider
{
  protected def commandExecute(meta: CommandMeta, command: AgentCommand): Task[Checked[AgentCommand.Response]]
  protected def commandOverview: Task[CommandHandlerOverview]
  protected def commandDetailed: Task[CommandHandlerDetailed[AgentCommand]]

  private implicit def implicitScheduler: Scheduler = scheduler

  final lazy val commandRoute: Route =
    authorizedUser(ValidUserPermission) { user =>
      post {
        pathEnd {
          withSizeLimit(entitySizeLimit) {
            entity(as[AgentCommand]) { command =>
              completeTask {
                commandExecute(CommandMeta(user), command)
              }
            }
          }
        }
      } ~
      get {
        respondWithHeader(`Cache-Control`(`max-age`(0))) {
          pathEnd {
            completeTask(
              commandOverview)
          } ~
          pathSingleSlash {
            val marshaller: ToResponseMarshaller[Seq[CommandRunOverview[AgentCommand]]] = {
              implicit val jsonCodec: Codec.AsObject[CommandRunOverview[AgentCommand]] =
                CommandRunOverview.jsonCodec[AgentCommand]
              implicitly[ToResponseMarshaller[Seq[CommandRunOverview[AgentCommand]]]]
            }
            completeTask(
              commandDetailed.map(_.commandRuns)
                .map(ToResponseMarshallable(_)(marshaller)))
          }
        }
      }
    }
}
