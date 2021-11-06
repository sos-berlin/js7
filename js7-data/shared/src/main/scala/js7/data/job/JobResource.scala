package js7.data.job

import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import js7.data.item.{ItemRevision, SignableSimpleItem}
import js7.data.value.expression.Expression

/** Job resources, for example environment variables. */
final case class JobResource(
  path: JobResourcePath,
  variables: Map[String, Expression] = Map.empty,
  env: Map[String, Expression] = Map.empty,
  itemRevision: Option[ItemRevision] = None)
extends SignableSimpleItem
{
  type Self = JobResource
  val companion = JobResource

  def withId(id: JobResourcePath) =
    copy(path = id)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)
}

object JobResource extends SignableSimpleItem.Companion[JobResource]
{
  type Key = JobResourcePath
  val Key = JobResourcePath

  type Path = JobResourcePath
  val Path = JobResourcePath

  val cls = classOf[JobResource]

  private implicit val configuration = withDefaults
  implicit val jsonCodec = deriveConfiguredCodec[JobResource]
}
