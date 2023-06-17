package js7.data.node

import js7.base.generic.GenericString

/** Relates to NodeId and is used to find the login password for the cluster node.
 *
 *  A Subagent's NodeName is the SubagentId,
 * a Controller cluster node's NodeName is the ControllerId. */
final case class NodeName private(string: String)
extends GenericString
{
  override def toString = s"NodeName:$string"
}

object NodeName extends GenericString.NonEmpty[NodeName]
{
  protected def unchecked(string: String): NodeName =
    new NodeName(string)
}
