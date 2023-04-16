package js7.data_for_java.agent

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.item.ItemRevision
import js7.data.subagent.SubagentId
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final case class JAgentRef(asScala: AgentRef)
extends JJsonable[JAgentRef] with JUnsignedSimpleItem
{
  type AsScala = AgentRef
  protected def companion = JAgentRef

  @Nonnull
  def path: AgentPath =
    asScala.path

  @Nonnull
  def directors: java.util.List[SubagentId] =
    asScala.directors.asJava

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]) =
    copy(asScala.withRevision(revision.toScala))
}

object JAgentRef extends JJsonable.Companion[JAgentRef]
{
  @Nonnull
  def of(
    @Nonnull path: AgentPath,
    @Nonnull directors: java.lang.Iterable[SubagentId])
  : JAgentRef =
    JAgentRef(AgentRef(path, directors = directors.asScala.toVector))

  @Nonnull
  def of(
    @Nonnull path: AgentPath,
    @Nonnull director: SubagentId)
  : JAgentRef =
    JAgentRef(AgentRef(path, directors = director :: Nil))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JAgentRef] =
    super.fromJson(jsonString)

  protected def jsonEncoder = AgentRef.jsonCodec
  protected def jsonDecoder = AgentRef.jsonCodec
}
