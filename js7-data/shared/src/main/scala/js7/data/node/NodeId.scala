package js7.data.node

import js7.base.annotation.javaApi
import js7.base.auth.UserId
import js7.base.generic.GenericString
import js7.base.problem.Checked

final case class NodeId private(string: String)
extends GenericString:
  override def toString = s"Node:$string"


object NodeId extends GenericString.NonEmpty[NodeId]:
  val primary: NodeId = NodeId.mayThrow("Primary")
  val backup: NodeId = NodeId.mayThrow("Backup")

  protected def unchecked(string: String): NodeId =
    new NodeId(string)

  override def checked(o: String): Checked[NodeId] =
    UserId.checked(o)
      .flatMap(u => super.checked(u.string))

  @javaApi
  def of(nodeId: String): NodeId =
    mayThrow(nodeId)
