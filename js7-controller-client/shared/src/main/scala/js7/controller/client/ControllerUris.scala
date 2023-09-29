package js7.controller.client

import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.base.web.Uris.{encodePath, encodeQuery, encodeSegment}
import js7.data.agent.AgentPath
import js7.data.event.EventId
import js7.data.order.OrderId
import scala.reflect.ClassTag

/**
 * URIs of the JS7 Controller.
 *
 * @author Joacim Zschimmer
 */
final class ControllerUris private(controllerUri: Uri):
  def overview = api()

  val command = api("/command")

  val session = api("/session")

  object order:
    def overview = api("/order")

    def add = api("/order")

    def apply(orderId: OrderId): Uri =
      api("/" + encodePath("order", orderId.string))

  object snapshot:
    def list: Uri =
      list(None)

    def list(eventId: Option[EventId]) =
      api("/" + encodePath("snapshot", ""), eventId.toList.map("eventId" -> _.toString)*)

  //def agentCommand(agentPath: AgentPath): Uri =
  //  agentForward(agentPath) / "command"

  def agentForward(agentPath: AgentPath): Uri =
    api("/agent-forward") / encodeSegment(agentPath.string)

  def api(query: (String, String)*): Uri =
    api("", query*)

  def api(path: String, query: (String, String)*): Uri =
    if path.nonEmpty && !path.startsWith("/") then throw new IllegalArgumentException("Controller URI path must start with a slash")
    Uri(
      controller("api" + path).string + encodeQuery(query*))

  def controller(path: String) = Uri(s"$controllerUri$path")

  override def toString = controllerUri.string

object ControllerUris:
  def apply(controllerUri: Uri): ControllerUris =
    new ControllerUris(Uri(controllerUri.string.stripSuffix("/") + "/"))

  private def encodeClass[A: ClassTag]: String =
    encodeClass(implicitClass[A])

  private def encodeClass(cls: Class[?]): String =
    require(cls != classOf[Nothing], "Missing return=CLASS")
    cls.simpleScalaName
