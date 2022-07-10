package js7.data.job

import io.circe.Codec
import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import js7.data.item.{ItemRevision, SignableSimpleItem, TrivialItemState}
import js7.data.value.expression.Expression

/** Job resources, for example environment variables. */
final case class JobResource(
  path: JobResourcePath,
  variables: Map[String, Expression] = Map.empty,
  env: Map[String, Expression] = Map.empty,
  itemRevision: Option[ItemRevision] = None)
extends SignableSimpleItem
with TrivialItemState[JobResource]
{
  type Self = JobResource
  val companion = JobResource
  val item = this

  def withId(id: JobResourcePath) =
    copy(path = id)

  def rename(path: JobResourcePath) =
    copy(path = path)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)
}

object JobResource
extends SignableSimpleItem.Companion[JobResource]
with TrivialItemState.Companion[JobResource]
{
  type Key = JobResourcePath
  val Key = JobResourcePath

  override type Path = JobResourcePath
  val Path = JobResourcePath

  val cls = classOf[JobResource]

  private implicit val configuration = withDefaults
  implicit val jsonCodec: Codec.AsObject[JobResource] =
    deriveConfiguredCodec[JobResource]
}
