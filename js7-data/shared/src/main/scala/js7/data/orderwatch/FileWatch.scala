package js7.data.orderwatch

import cats.syntax.semigroup.*
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Codec, Decoder, DecodingFailure}
import java.util.regex.{Matcher, Pattern}
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.item.{InventoryItemPath, ItemRevision}
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.*
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import js7.data.value.expression.scopes.{EnvScope, NowScope}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{ObjectValue, StringValue}
import js7.data.workflow.WorkflowPath
import scala.collection.View
import scala.collection.immutable.Map.Map2
import scala.concurrent.duration.FiniteDuration

final case class FileWatch(
  path: OrderWatchPath,
  workflowPath: WorkflowPath,
  agentPath: AgentPath,
  directoryExpr: Expression,
  pattern: Option[SimplePattern] = None,
  orderExpr: Option[Expression] = None,
  orderIdExpression: Option[Expression] = None, // COMPATIBLE with v2.7.3
  delay: FiniteDuration = ZeroDuration,
  itemRevision: Option[ItemRevision] = None)
extends OrderWatch:

  protected type Self = FileWatch
  val companion: FileWatch.type = FileWatch

  def rename(path: OrderWatchPath): FileWatch =
    copy(path = path)

  def withRevision(revision: Option[ItemRevision]): FileWatch =
    copy(itemRevision = revision)

  override val referencedItemPaths: View[InventoryItemPath] =
    View(agentPath, workflowPath)

  def externalToOrderAndPlanId(
    externalOrderName: ExternalOrderName,
    legacyOrderId: Option[OrderId],
    now: Timestamp)
  : Checked[(OrderId, PlanId)] =
    val relativePath = externalOrderName.string
    val matcher = matchFilename(relativePath)
    if !matcher.matches() then
      Left(FileWatch.FileWatchPatternDoesntMatchProblem(path / externalOrderName))
    else
      val checkedDefaultOrderId = OrderId.checked(s"file:${path.string}:$relativePath")
      val scope = FileWatchScope(path, matcher) |+| EnvScope |+| NowScope(now)
      (orderExpr, orderIdExpression) match
        case (Some(orderExpr), None) =>
          orderExpr.evalAs(ObjectValue, scope).flatMap: obj =>
            for
              orderId <- obj.nameToValue.get("orderId").fold(checkedDefaultOrderId):
                _.asString.flatMap(OrderId.checked)
              planId <- obj.nameToValue.get("planId").fold(Checked(PlanId.Global)):
                _.asList.flatMap:
                  case Seq() => Right(PlanId.Global)
                  case Seq(StringValue(planSchemaId), StringValue(planKey)) =>
                    for
                      planSchemaId <- PlanSchemaId.checked(planSchemaId)
                      planKey <- PlanKey.checked(planKey)
                    yield
                      planSchemaId / planKey
                  case _ => Left(Problem("planId must be a list of two strings"))
            yield
              orderId -> planId

        case (None, Some(legacyOrderIdExpr)) =>
          legacyOrderId.map(orderId => Checked(orderId -> PlanId.Global)).getOrElse:
            legacyOrderIdExpr.evalAsString(scope).flatMap(OrderId.checked).map(_ -> PlanId.Global)

        case (None, None) =>
          checkedDefaultOrderId.map(_ -> PlanId.Global)

        case _ => Left(Problem("orderExpr cannot be combined with legacy orderIdExpression"))

  def matchesFilename(filename: String): Boolean =
    matchFilename(filename).matches()

  private def matchFilename(filename: String): Matcher =
    pattern.fold(defaultPattern)(_.pattern).matcher(filename)


object FileWatch extends OrderWatch.Companion[FileWatch]:

  val cls: Class[FileWatch] = classOf[FileWatch]

  override type Path = OrderWatchPath
  override val Path: OrderWatchPath.type = OrderWatchPath

  val FileArgumentName = "file"
  private val defaultPattern = Pattern.compile("[^.].*")

  implicit val jsonCodec: Codec.AsObject[FileWatch] =
    val decoder: Decoder[FileWatch] =
      c => for
        path <- c.get[OrderWatchPath]("path")
        workflowPath <- c.get[WorkflowPath]("workflowPath")
        agentPath <- c.get[AgentPath]("agentPath")
        directoryExpr <- c.get[Expression]("directoryExpr")
          .orElse(
            c.get[String]("directory")/*COMPATIBLE with v2.0.1*/.map(Expression.StringConstant(_)))
        pattern <- c.get[Option[SimplePattern]]("pattern")
        orderExpr <- c.get[Option[Expression]]("orderExpr")
        orderIdExpression <- c.get[Option[Expression]]("orderIdExpression")
        _ <-
          if orderIdExpression.isDefined && orderExpr.isDefined then
            Left(DecodingFailure("orderExpr cannot be combined with legacy orderIdExpression", c.history))
          else
            Right(())
        delay <- c.getOrElse[FiniteDuration]("delay")(ZeroDuration)
        itemRevision <- c.get[Option[ItemRevision]]("itemRevision")
      yield FileWatch(path, workflowPath, agentPath,
        directoryExpr, pattern, orderExpr, orderIdExpression, delay, itemRevision)

    Codec.AsObject.from(
      decoder,
      deriveEncoder[FileWatch])

  intelliJuseImport(FiniteDurationJsonEncoder)

  final case class FileWatchPatternDoesntMatchProblem(externalOrderKey: ExternalOrderKey)
  extends Problem.Coded:
    def arguments = Map2(
      "orderWatchPath", externalOrderKey.orderWatchPath.string,
      "externalOrderName", externalOrderKey.name.string)

  object FileWatchPatternDoesntMatchProblem extends Problem.Coded.Companion
