package js7.cluster.web

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives.*
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.CirceUtils.RichCirceEither
import js7.base.problem.Checked
import js7.base.utils.FutureCompletion
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.web.ClusterRoute.*
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonSupport.*
import js7.common.akkahttp.StandardMarshallers.*
import js7.data.cluster.{ClusterCommand, ClusterNodeState, ClusterState, ClusterWatchingCommand}
import js7.data.event.Stamped
import js7.data.node.NodeId
import js7.journal.watch.FileEventWatch
import monix.eval.Task
import monix.execution.Scheduler

trait ClusterRoute extends ClusterWatchRequestRoute:

  protected def scheduler: Scheduler
  protected def actorSystem: ActorSystem
  protected def checkedClusterState: Task[Checked[Stamped[ClusterState]]]
  protected def clusterNodeIsBackup: Boolean
  protected def nodeId: NodeId
  protected def executeClusterCommand(cmd: ClusterCommand): Task[Checked[ClusterCommand.Response]]
  protected def executeClusterWatchingCommand(cmd: ClusterWatchingCommand): Task[Checked[Unit]]
  protected def eventWatch: FileEventWatch

  private implicit def implicitScheduler: Scheduler = scheduler

  // TODO Abort POST with error when shutting down
  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  protected final lazy val clusterRoute =
    authorizedUser(ValidUserPermission) { user =>
      get {
        pathEnd {
          parameter("return".?) { maybeReturn =>
            completeTask(
              checkedClusterState
                .map(_.map[ToResponseMarshallable] { case stamped @ Stamped(_, _, clusterState) =>
                  if maybeReturn contains "ClusterNodeState" then
                    stamped.copy(value =
                      ClusterNodeState(nodeId, isBackup = clusterNodeIsBackup, clusterState))
                  else
                    stamped.asJson
                }))
          }
        } ~
        path("clusterWatchMessage"):
          clusterWatchMessageRoute(user.id)
      } ~
      post:
        path("command"):
          entity(as[Json]) { json =>
            commandJsonCodec
              .decodeJson(json)
              .toChecked
              .fold(complete(_), {
                case cmd: ClusterCommand =>
                  completeTask(
                    executeClusterCommand(cmd))

                case cmd: ClusterWatchingCommand =>
                  completeTask(
                    executeClusterWatchingCommand(cmd)
                      .rightAs(JsonObject.empty))
            })
          }
    }

object ClusterRoute:
  private val commandJsonCodec =
    ClusterCommand.jsonCodec | ClusterWatchingCommand.jsonCodec
  //private val logger = Logger[this.type]
