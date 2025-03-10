package js7.launcher.forjava.internal

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.controller.ControllerId
import js7.data.value.Value
import js7.data.workflow.position.Label
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.order.JOrder
import js7.data_for_java.vavr.VavrConverters.*
import js7.data_for_java.workflow.JWorkflow
import js7.launcher.internal.InternalJob.Step
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

trait JavaJobStep extends JavaWrapper:
  type AsScala = Step

  @Nonnull
  def asScala: Step

  // Scala 3.6.2 erases type parameters from lazy val. So we publish via a def-function.
  private lazy val _arguments: java.util.Map[String, Value] =
    asScala.arguments.asJava

  @Nonnull
  final def arguments: java.util.Map[String, Value] =
    _arguments

  @Nonnull
  final lazy val order: JOrder =
    JOrder(asScala.order)

  @Nonnull
  final def jobName: String =
    asScala.processOrder.simpleJobName

  @Nonnull
  final def jobExecutionCount: Int =
    asScala.processOrder.jobExecutionCount

  @Nonnull
  final lazy val workflow: JWorkflow =
    JWorkflow(asScala.workflow)

  @Nonnull
  def instructionLabel: Optional[Label] =
    asScala.processOrder.instructionLabel.toJava

  @Nonnull
  def controllerId: ControllerId =
    asScala.processOrder.controllerId

  /** Read any (maybe undeclared) names value.
    * <p>
    * This is like `$name` in the expression language.
    * This mini history is scanned.
    * In case of an declared but missing Order argument,
    * the default values is returned (in case it exists).
    * <p>
    * Does not return arguments declared in the job.
    * <p>
    * Returns `None` if the `name` is unknown.
    * Returns `Some(Left)` only on lazy evaluation.
    * */
  @Nonnull
  def namedValue(name: String): Optional[VEither[Problem, Value]] =
    asScala.processOrder.scope.namedValue(name)
      .map(_.toVavr).toJava
