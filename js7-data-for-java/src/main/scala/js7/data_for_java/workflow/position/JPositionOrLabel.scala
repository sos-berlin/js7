package js7.data_for_java.workflow.position

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.position.{Label, Position, PositionOrLabel}
import js7.data_for_java.common.{JJsonable, JavaWrapper}
import js7.data_for_java.vavr.VavrConverters.*
import scala.jdk.CollectionConverters.*

sealed trait JPositionOrLabel extends JavaWrapper:

  type AsScala <: PositionOrLabel
  def toJson: String

object JPositionOrLabel:
  def apply(underlying: PositionOrLabel): JPositionOrLabel =
    underlying match
      case o: Position => JPosition(o)
      case o: Label => JLabel(o)

  protected def jsonEncoder = PositionOrLabel.jsonEncoder
  protected def jsonDecoder = PositionOrLabel.jsonDecoder

/** Position in a Workflow. */
final case class JPosition(asScala: Position)
extends JJsonable[JPosition] with JPositionOrLabel:
  type AsScala = Position
  protected def companion = JPosition

  /** The Position as a List of uneven size of Integers and Strings.
    * The first element is the instruction number.
    * Pairs of BranchIds and instruction numbers may follow, if the position is deep in the workflow.
    */
  @Nonnull
  def toList: java.util.List[Any] =
    asScala.toSeq.asJava

object JPosition extends JJsonable.Companion[JPosition]:
  type AsScala = Position

  @javaApi @Nonnull
  def fromList(@Nonnull position: java.util.List[Any]): VEither[Problem, JPosition] =
    Position.fromSeq(position.asScala.toSeq).map(apply).toVavr

  override def fromJson(jsonString: String): VEither[Problem, JPosition] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Position.jsonEncoder
  protected def jsonDecoder = Position.jsonDecoder

final case class JLabel(asScala: Label)
extends JJsonable[JLabel] with JPositionOrLabel:
  type AsScala = Label
  protected val companion: JLabel.type = JLabel
object JLabel extends JJsonable.Companion[JLabel]:
  type AsScala = Label

  def of(label: String): JLabel =
    JLabel(Label(label))

  protected def jsonEncoder = Label.jsonEncoder
  protected def jsonDecoder = Label.jsonDecoder
