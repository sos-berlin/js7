package js7.proxy.javaapi.data.workflow.position

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.position.Position
import js7.proxy.javaapi.data.common.JJsonable
import js7.proxy.javaapi.data.common.VavrConverters._
import scala.jdk.CollectionConverters._

/** Position in a Workflow. */
final case class JPosition(underlying: Position)
extends JJsonable[JPosition]
{
  protected type Underlying = Position
  protected def companion = JPosition

  /** The Position as a List of uneven size of Integers and Strings.
    * The first element is the instruction number.
    * Pairs of BranchIds and instruction numbers may follow, if the position is deep in the workflow.
    */
  def toList: java.util.List[Any] =
    underlying.toSeq.asJava
}

object JPosition extends JJsonable.Companion[JPosition]
{
  @javaApi
  def fromList(position: java.util.List[Any]): VEither[Problem, JPosition] =
    Position.fromSeq(position.asScala.toSeq).map(apply).toVavr

  override def fromJson(jsonString: String): VEither[Problem, JPosition] =
    super.fromJson(jsonString)

  val jsonEncoder = Position.jsonEncoder
  val jsonDecoder = Position.jsonDecoder
}
