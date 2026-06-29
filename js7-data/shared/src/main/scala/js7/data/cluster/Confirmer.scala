package js7.data.cluster

import js7.base.annotation.javaApi
import js7.base.generic.GenericString

/** External "manual" confirmer of a ClusterNodeLostEvent.
  *
  * A name of a subject, external to the Engine, who has confirmed that
  * a node is lost and dead (no longer running).
  *
  * When the ClusterWatch cannot be sure about it's okay to acknowledge a `ClusterNodeLostEvent`,
  * then an external Confirmer (for example, a JOC user)
  * may confirm that a cluster node is really lost and dead.
  *
  * This is used for logging. The name of the confimer is not checked.
  *
  * The external confirmer is also called (historically) manual confirmer.
  *
  * @param string the name of the external confirmer.
  */
final case class Confirmer(string: String) extends GenericString:
  override def toString = s"Confimer:$string"


object Confirmer extends GenericString.Checked_[Confirmer]:
  protected def unchecked(string: String) = new Confirmer(string)

  @javaApi
  def of(string: String): Confirmer =
    Confirmer(string)
