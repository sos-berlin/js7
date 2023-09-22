package js7.data.job

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
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
  protected type Self = JobResource
  val companion: JobResource.type = JobResource
  val item: JobResource = this

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

  implicit val jsonCodec: Codec.AsObject[JobResource] =
    deriveConfiguredCodec[JobResource]
}
