package js7.data.agent

import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.semiauto.deriveConfiguredEncoder
import io.circe.{Codec, Decoder}
import js7.base.circeutils.CirceUtils._
import js7.base.problem.{Checked, Problem}
import js7.base.web.Uri
import js7.data.item.{ItemRevision, UnsignedSimpleItem}
import js7.data.subagent.{SubagentId, SubagentItem}

final case class AgentRef(
  path: AgentPath,
  directors: Seq[SubagentId],
  uri: Option/*COMPATIBLE with v2.1*/[Uri] = None,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem
{
  protected type Self = AgentRef
  val companion = AgentRef

  private def checked: Checked[this.type] =
    for {
      _ <- AgentPath.checked(path.string)
      _ <-
        if (directors.isEmpty && uri.isEmpty)
          Left(Problem.pure(s"Missing Director in $path"))
        else if (directors.nonEmpty && uri.nonEmpty)
          Left(Problem.pure(s"AgentRef.directors cannot be used with .uri"))
        else if (directors.sizeIs > 1)
          Left(Problem.pure(s"Only one Agent Director is allowed in $path"))
        else
          Checked.unit
    } yield this

  def director = directors.headOption

  def rename(path: AgentPath) =
    copy(path = path)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  override def referencedItemPaths = directors.view

  def convertFromLegacy: Checked[(AgentRef, Option[SubagentItem])] =
    this match {
      case AgentRef(agentPath, directors, Some(uri), itemRevision) =>
        if (directors.nonEmpty)
          Left(Problem.pure("Invalid AgentRef: both directors and uri?"))
        else {
          // COMPATIBLE with v2.2
          val subagentItem = SubagentItem(
            SubagentId.legacyLocalFromAgentPath(agentPath),
            agentPath, uri,
            itemRevision = Some(ItemRevision(1)))

          val agentRef = AgentRef(agentPath,
            directors = Seq(subagentItem.id),
            itemRevision = itemRevision)

          Right((agentRef, Some(subagentItem)))
      }

      case agentRef @ AgentRef(_, _, None, _) =>
        Right((agentRef, None))
    }
}

object AgentRef extends UnsignedSimpleItem.Companion[AgentRef]
{
  val cls = classOf[AgentRef]

  type Key = AgentPath
  val Key = AgentPath

  type Path = AgentPath
  val Path = AgentPath

  implicit val jsonCodec = {
    val jsonDecoder: Decoder[AgentRef] =
      c => for {
        path <- c.get[AgentPath]("path")
        directors <- c.getOrElse[Vector[SubagentId]]("directors")(Vector.empty)
        uri <- c.get[Option[Uri]]("uri")
        rev <- c.get[Option[ItemRevision]]("itemRevision")
        agentRef <- AgentRef(path, directors, uri, rev)
          .checked.toDecoderResult(c.history)
      } yield agentRef

    implicit val configuration = withDefaults
    Codec.AsObject.from(jsonDecoder, deriveConfiguredEncoder[AgentRef])
  }
}
