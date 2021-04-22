package js7.data.job

import io.circe.Codec
import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import js7.data.item.{ItemRevision, SignableSimpleItem}
import js7.data.value.expression.Expression.ObjectExpression

/** Job resources, for example environment variables. */
final case class JobResource(
  id: JobResourcePath,
  env: ObjectExpression = ObjectExpression.empty,
  //dependsOn: Seq[JobResourcePath] = Nil,
  itemRevision: Option[ItemRevision] = None)
extends SignableSimpleItem
{
  type Self = JobResource
  val companion = JobResource

  def withId(id: JobResourcePath) =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)
}

object JobResource extends SignableSimpleItem.Companion[JobResource]
{
  type Key = JobResourcePath
  val Key = JobResourcePath
  val cls = classOf[JobResource]

  private implicit val configuration = withDefaults
  implicit val jsonCodec: Codec.AsObject[JobResource] = deriveConfiguredCodec[JobResource]
}
