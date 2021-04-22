package js7.data_for_java.jobresource

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.value.expression.Expression.ObjectExpression
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JSignableItem
import js7.data_for_java.value.JExpression
import scala.jdk.CollectionConverters._

final case class JJobResource(asScala: JobResource)
extends JJsonable[JJobResource] with JSignableItem
{
  protected type AsScala = JobResource
  protected def companion = JJobResource

  @Nonnull
  def path: JobResourcePath =
    asScala.path

  @Nonnull
  def env: java.util.Map[String, JExpression] =
    asScala.env.nameToExpr
      .view
      .mapValues(JExpression(_))
      .toMap
      .asJava
}

object JJobResource extends JJsonable.Companion[JJobResource]
{
  @Nonnull
  def of(
    @Nonnull path: JobResourcePath,
    @Nonnull env: java.util.Map[String, JExpression])
  : JJobResource =
    JJobResource(
      JobResource(path, ObjectExpression(env.asScala.view.mapValues(_.asScala).toMap)))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JJobResource] =
    super.fromJson(jsonString)

  protected def jsonEncoder = JobResource.jsonCodec
  protected def jsonDecoder = JobResource.jsonCodec
}
