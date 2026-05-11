package js7.data.node

import js7.base.annotation.javaApi
import js7.data.controller.ControllerId

sealed trait Js7ServerGroupId

object Js7ServerGroupId:

  @javaApi @Deprecated
  def controller(controllerId: ControllerId): Engine =
    engine(controllerId)

  /** @param controllerId The whole JS7 JobScheduler Engine is identified by it's ControllerId. */
  @javaApi
  def engine(controllerId: ControllerId): Engine =
    Engine(controllerId)

  @javaApi
  def proxy(name: String): Proxy =
    Proxy(name)


  final case class Proxy(name: String) extends Js7ServerGroupId:
    override def toString = if name.isEmpty then "Proxy" else s"Proxy:$name"

  object Proxy:
    val empty: Proxy = Proxy("")

  type Provider = Provider.type
  case object Provider extends Js7ServerGroupId:
    val name = "Provider"
    override def toString = "Provider"


  final case class Engine(controllerId: ControllerId) extends Js7ServerGroupId:
    override def toString = s"Engine:${controllerId.string}"


  object Engine:
    @javaApi
    def of(controllerId: ControllerId): Engine =
      Engine(controllerId)


type GroupAndServerId = (groupId: Js7ServerGroupId, serverId: Js7ServerId)
