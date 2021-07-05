package js7.data_for_java.workflow.position

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.position.Position
import js7.data_for_java.common.JJsonable
import js7.data_for_java.vavr.VavrConverters._
import scala.jdk.CollectionConverters._

/** Position in a Workflow. */
final case class JPosition(asScala: Position)
extends JJsonable[JPosition]
{
  protected type AsScala = Position
  protected def companion = JPosition

  /** The Position as a List of uneven size of Integers and Strings.
    * The first element is the instruction number.
    * Pairs of BranchIds and instruction numbers may follow, if the position is deep in the workflow.
    */
  @Nonnull
  def toList: java.util.List[Any] =
    asScala.toSeq.asJava
}

object JPosition extends JJsonable.Companion[JPosition]
{
  @javaApi @Nonnull
  def fromList(@Nonnull position: java.util.List[Any]): VEither[Problem, JPosition] =
    Position.fromSeq(position.asScala.toSeq).map(apply).toVavr

  override def fromJson(jsonString: String): VEither[Problem, JPosition] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Position.jsonEncoder
  protected def jsonDecoder = Position.jsonDecoder
}
