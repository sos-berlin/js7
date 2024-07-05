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
extends SignableSimpleItem, TrivialItemState[JobResource]:

  val companion: JobResource.type = JobResource

  def withId(id: JobResourcePath): JobResource =
    copy(path = id)

  def rename(path: JobResourcePath): JobResource =
    copy(path = path)

  def withRevision(revision: Option[ItemRevision]): JobResource =
    copy(itemRevision = revision)


object JobResource
extends SignableSimpleItem.Companion[JobResource],
  TrivialItemState.Companion[JobResource]:

  type Key = JobResourcePath
  def Key: JobResourcePath.type = JobResourcePath

  override type Path = JobResourcePath
  val Path: JobResourcePath.type = JobResourcePath

  val cls: Class[JobResource] = classOf[JobResource]

  implicit val jsonCodec: Codec.AsObject[JobResource] =
    deriveConfiguredCodec[JobResource]
