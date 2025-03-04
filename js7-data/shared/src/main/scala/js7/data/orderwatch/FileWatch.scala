package js7.data.orderwatch

import cats.syntax.semigroup.*
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Codec, Decoder}
import java.util.regex.{Matcher, Pattern}
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.item.{InventoryItemPath, ItemRevision}
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.*
import js7.data.plan.PlanId
import js7.data.value.expression.Expression.expr
import js7.data.value.expression.scopes.{EnvScope, NowScope}
import js7.data.value.expression.{Expression, Scope}
import js7.data.workflow.WorkflowPath
import scala.collection.View
import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable.Map.Map2

final case class FileWatch(
  path: OrderWatchPath,
  workflowPath: WorkflowPath,
  agentPath: AgentPath,
  directoryExpr: Expression,
  pattern: Option[SimplePattern] = None,
  orderIdExpression: Option[Expression] = None,
  planIdExpr: Option[Expression] = None,
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

  def evalPlanIdExpr(scope: Scope): Checked[PlanId] =
    PlanId.evalPlanIdExpr(planIdExpr getOrElse PlanId.GlobalPlanIdExpr, scope)

  def externalToOrderId(externalOrderName: ExternalOrderName): Checked[OrderId] =
    val relativePath = externalOrderName.string
    lazy val default = OrderId.checked(s"file:${path.string}:$relativePath")
    val matcher = matchFilename(relativePath)
    if !matcher.matches() then
      Left(FileWatch.FileWatchPatternDoesntMatchProblem(path / externalOrderName))
    else
      orderIdExpression match
        case None => default
        case Some(expr) =>
          evalAsString(path, expr, matcher)
            .flatMap(OrderId.checked)

  def matchesFilename(filename: String): Boolean =
    matchFilename(filename).matches()

  private def matchFilename(filename: String): Matcher =
    pattern.fold(defaultPattern)(_.pattern).matcher(filename)

  private def evalAsString(orderWatchPath: OrderWatchPath, expression: Expression, matchedMatcher: Matcher) =
    expression.evalAsString:
      FileWatchScope(orderWatchPath, matchedMatcher) |+| NowScope() |+| EnvScope


object FileWatch extends OrderWatch.Companion[FileWatch]:

  val cls: Class[FileWatch] = classOf[FileWatch]

  override type Path = OrderWatchPath
  override val Path: OrderWatchPath.type = OrderWatchPath

  val FileArgumentName = "file"
  private val defaultPattern = Pattern.compile("[^.].*")
  val todayPlanIdExpr: Expression =
    expr"[ 'DailyPlan', match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1') ]"

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
        orderIdExpression <- c.get[Option[Expression]]("orderIdExpression")
        planIdExpr <- c.get[Option[Expression]]("planIdExpr")
        delay <- c.getOrElse[FiniteDuration]("delay")(ZeroDuration)
        itemRevision <- c.get[Option[ItemRevision]]("itemRevision")
      yield FileWatch(path, workflowPath, agentPath,
        directoryExpr, pattern, orderIdExpression, planIdExpr, delay, itemRevision)

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
