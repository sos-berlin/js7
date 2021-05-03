package js7.executor.forjava.internal

import java.util.Optional
import javax.annotation.Nonnull
import js7.data.value.Value
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.order.JOrder
import js7.data_for_java.workflow.JWorkflow
import js7.executor.internal.InternalJob.Step
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

trait JavaJobStep extends JavaWrapper
{
  type AsScala = Step

  @Nonnull
  def asScala: Step

  @Nonnull
  final lazy val order =
    JOrder(asScala.order)

  @Nonnull
  final lazy val workflow =
    JWorkflow(asScala.workflow)

  @Nonnull
  final lazy val jobName: String =
    asScala.processOrder.jobName

  @Nonnull
  final lazy val arguments: java.util.Map[String, Value] =
    asScala.arguments.asJava

  /** Read any (maybe undeclared) names value.
    * <p>
    * This is like `$name` in the expression language.
    * This mini history is scanned.
    * In case of an declared but missing Order argument,
    * the default values is returned (in case it exists).
    * <p>
    * Does not return arguments declared in the job.
    * */
  @Nonnull
  def namedValue(name: String): Optional[Value] =
    asScala.processOrder.scope.namedValue(name).toJava
}
