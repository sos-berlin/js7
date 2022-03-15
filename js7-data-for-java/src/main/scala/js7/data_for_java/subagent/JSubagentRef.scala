package js7.data_for_java.subagent

import io.vavr.control.{Either => VEither}
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.item.ItemRevision
import js7.data.subagent.{SubagentId, SubagentRef}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import scala.jdk.OptionConverters._

final case class JSubagentRef(asScala: SubagentRef)
extends JJsonable[JSubagentRef] with JUnsignedSimpleItem
{
  protected type AsScala = SubagentRef
  protected def companion = JSubagentRef

  @Nonnull
  def path = id

  @Nonnull
  def id: SubagentId =
    asScala.id

  @Nonnull
  def agentPath: AgentPath =
    asScala.agentPath

  @Nonnull
  def uri: Uri =
    asScala.uri

  def disabled: Boolean =
    asScala.disabled

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]) =
    copy(asScala.withRevision(revision.toScala))
}

object JSubagentRef extends JJsonable.Companion[JSubagentRef]
{
  @Nonnull
  def of(
    @Nonnull id: SubagentId,
    @Nonnull agentPath: AgentPath,
    @Nonnull uri: Uri,
    disabled: Boolean)
  = JSubagentRef(SubagentRef(id, agentPath, uri, disabled = disabled))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JSubagentRef] =
    super.fromJson(jsonString)

  protected def jsonEncoder = SubagentRef.jsonCodec
  protected def jsonDecoder = SubagentRef.jsonCodec
}
