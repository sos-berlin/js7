package js7.data.agent

import js7.base.annotation.javaApi
import js7.data.cluster.ClusterId
import js7.data.delegate.DelegateId
import js7.data.item.UnsignedSimpleItemPath

/**
  * @author Joacim Zschimmer
  */
final case class AgentPath private(string: String)
extends UnsignedSimpleItemPath, DelegateId, ClusterId, AtControllerOrAgent:

  protected type Self = AgentPath

  val companion: AgentPath.type = AgentPath

  def maybeAgentPath: Some[AgentPath] =
    Some(this)

  override def toString = s"Agent:$string"  // instead of AgentRef:


object AgentPath
extends DelegateId.Companion[AgentPath], UnsignedSimpleItemPath.Companion[AgentPath]:
  type Item = AgentRef

  /** Internal use only. */
  private[js7] val empty = new AgentPath("")

  override val itemTypeName = "AgentRef"  // May deadlock: AgentRef.typeName
  override val pathTypeName = "Agent"

  protected def unchecked(string: String) = new AgentPath(string)

  @javaApi
  def of(validName: String): AgentPath =
    mayThrow(validName)


sealed trait AtControllerOrAgent:
  def maybeAgentPath: Option[AgentPath]


case object AtController extends AtControllerOrAgent:
  def maybeAgentPath: None.type = None
