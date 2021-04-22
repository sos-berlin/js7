package js7.controller.web.controller.api

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.akkahttp.StandardDirectives.remainingPath
import js7.common.akkahttp.StandardMarshallers.checkedToResponseMarshaller
import js7.controller.web.common.ControllerRouteProvider
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait AgentRefRoute extends ControllerRouteProvider
{
  protected def pathToAgentRefState: Task[Checked[Map[AgentPath, AgentRefState]]]

  private implicit def implicitScheduler: Scheduler = scheduler

  final val agentRefRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        //pathEnd {
        //  completeTask(
        //    itemApi.overview[AgentRef])
        //} ~
          pathSingleSlash {
            parameter("return".?) {
              case None =>
                completeTask[Checked[Iterable[AgentPath]]](
                  pathToAgentRefState.map(_.map(_.keys)))

              case Some("AgentRef") =>
                completeTask[Checked[Iterable[AgentRef]]](
                  pathToAgentRefState.map(_.map(_.values.map(_.agentRef))))

              case Some("AgentRefState") =>
                completeTask[Checked[Iterable[AgentRefState]]](
                  pathToAgentRefState.map(_.map(_.values)))

              case _ =>
                complete(NotFound)
            }
          } ~
          path(remainingPath[AgentPath]) { name =>
            parameter("return".?) {
              case None | Some("AgentRefState") =>
                completeTask[Checked[AgentRefState]](
                  pathToAgentRefState.map(_.flatMap(_.checked(name))))

              case Some("AgentRef") =>
                completeTask[Checked[AgentRef]](
                  pathToAgentRefState.map(_.flatMap(_.checked(name).map(_.agentRef))))

              case _ =>
                complete(NotFound)
            }
          }
      }
    }
}

object AgentRefRoute {
  intelliJuseImport(() => checkedToResponseMarshaller(null))
}
