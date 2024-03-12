package js7.cluster.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.CirceUtils.RichCirceEither
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.web.ClusterRoute.*
import js7.common.pekkohttp.CirceJsonSupport.*
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.common.pekkohttp.StandardMarshallers.*
import js7.data.cluster.{ClusterCommand, ClusterNodeState, ClusterState, ClusterWatchingCommand}
import js7.data.event.Stamped
import js7.data.node.NodeId
import js7.journal.watch.FileEventWatch
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.marshalling.ToResponseMarshallable
import org.apache.pekko.http.scaladsl.server.Directives.*

trait ClusterRoute extends ClusterWatchRequestRoute:

  protected def ioRuntime: IORuntime
  protected def actorSystem: ActorSystem
  protected def checkedClusterState: IO[Checked[Stamped[ClusterState]]]
  protected def clusterNodeIsBackup: Boolean
  protected def nodeId: NodeId
  protected def executeClusterCommand(cmd: ClusterCommand): IO[Checked[ClusterCommand.Response]]
  protected def executeClusterWatchingCommand(cmd: ClusterWatchingCommand): IO[Checked[Unit]]
  protected def eventWatch: FileEventWatch

  private given IORuntime = ioRuntime

  protected final lazy val clusterRoute =
    authorizedUser(ValidUserPermission) { user =>
      get {
        pathEnd {
          parameter("return".?) { maybeReturn =>
            completeIO(
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
                  completeIO(
                    executeClusterCommand(cmd))

                case cmd: ClusterWatchingCommand =>
                  completeIO(
                    executeClusterWatchingCommand(cmd)
                      .rightAs(JsonObject.empty))
            })
          }
    }


object ClusterRoute:
  private val commandJsonCodec =
    ClusterCommand.jsonCodec | ClusterWatchingCommand.jsonCodec
  //private val logger = Logger[this.type]
