package com.sos.jobscheduler.master.web.master.api.graphql

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.generic.{GenericInt, GenericString}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.{FileBasedId, TypedPath, VersionId}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, End, Execute, ForkJoin, Gap, Goto, If, IfNonZeroReturnCodeGoto, Instructions, Offer}
import com.sos.jobscheduler.data.workflow.position.WorkflowPosition
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow, WorkflowPath}
import java.util.regex.Pattern
import sangria.ast
import sangria.marshalling.FromInput.CoercedScalaResult
import sangria.schema.{Argument, Field, InputType, IntType, InterfaceType, LeafType, ListType, LongType, Named, NullableType, ObjectType, OptionInputType, OptionType, OutputType, ScalarType, Schema, StringType, UnmodifiedType, fields, interfaces}
import sangria.util.tag.@@
import sangria.validation.{StringCoercionViolation, ValueCoercionViolation, Violation}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
private[graphql] object MasterGraphqlSchema
{
  private type SomeType[T] = InputType[T @@ CoercedScalaResult] with OutputType[T] with LeafType with NullableType with UnmodifiedType with Named

  private implicit def toSome[A](a: A): Option[A] = Some(a)

  private val IntegerOrStringType: ScalarType[Any] = {
    val violation = valueCoercionViolation("Integer or String expected")
    ScalarType[Any](
      "IntegerOrString",
      "Integer or String",
      coerceOutput = {
        case (v: String, _) ⇒ v
        case (v: Int, _) ⇒ v
      },
      coerceInput = {
        case o: ast.StringValue ⇒ Right(o)
        case o: ast.IntValue ⇒ Right(o)
        case _ ⇒ Left(violation)
      },
      coerceUserInput = {
        case o: String ⇒ Right(o)
        case o: Int ⇒ Right(o)
        case _ ⇒ Left(violation)
      })
  }

  private implicit val PatternType = stringEquivalentType[Pattern](str ⇒ Checked.catchNonFatal(Pattern.compile(str)), _.toString,
    "Pattern", "Regular expression pattern")
  private implicit val OrderIdType      = genericStringType[OrderId]("Identifies the Order in JobScheduler")
  private implicit val WorkflowPathType = genericStringType[WorkflowPath]("Path of Workflow (String)")
  private implicit val AgentPathType    = genericStringType[AgentPath]("Path of Agent (String)")
  private implicit val ExecutablePathType = genericStringType[ExecutablePath]("Path of an executable (String)")
  private implicit val WorkflowJobNameType = genericStringType[WorkflowJob.Name]("Job name (String)")
  private implicit val VersionIdType    = genericStringType[VersionId]("Version identifier (String)")
  private implicit val ReturnCodeType   = genericIntType[ReturnCode]("Return code")

  private def genericStringType[A <: GenericString](description: String)(implicit A: GenericString.Companion[A]): ScalarType[A] =
    stringEquivalentType[A](str ⇒ A.checked(str), _.string, A.name.replace('.', '_'), description)

  private def stringEquivalentType[A](fromString: String ⇒ Checked[A], toString: A ⇒ String, name: String, description: String): ScalarType[A] = {
    val violation = valueCoercionViolation(s"'$name' string expected")
    ScalarType[A](
      name = name,
      description = description,
      coerceOutput = (v, _) ⇒ toString(v),
      coerceInput = {
        case o: ast.StringValue ⇒ fromChecked(fromString(o.value), valueCoercionViolation)
        case _ ⇒ Left(violation)
      },
      coerceUserInput = {
        case o: String ⇒ fromChecked(fromString(o), valueCoercionViolation)
        case _ ⇒ Left(violation)
      })
  }

  private def genericIntType[A <: GenericInt](description: String)(implicit A: GenericInt.Companion[A]): ScalarType[A] =
    intEquivalentType[A](i ⇒ Valid(A(i)), _.number, A.name, description)

  private def intEquivalentType[A](fromInt: Int ⇒ Checked[A], toInt: A ⇒ Int, name: String, description: String): ScalarType[A] = {
    val violation = valueCoercionViolation(s"'$name' string expected")
    ScalarType[A](
      name = name,
      description = description,
      coerceOutput = (v, _) ⇒ toInt(v),
      coerceInput = {
        case o: ast.IntValue ⇒ fromChecked(fromInt(o.value), valueCoercionViolation)
        case _ ⇒ Left(violation)
      },
      coerceUserInput = {
        case o: Int ⇒ fromChecked(fromInt(o), valueCoercionViolation)
        case _ ⇒ Left(violation)
      })
  }

  private def fromChecked[A, V <: Violation](checked: Checked[A], toViolation: String ⇒ V): Either[Violation, A] =
    checked match {
      case Invalid(problem) ⇒ Left(toViolation(problem.toString))
      case Valid(a) ⇒ Right(a)
    }

  private def valueCoercionViolation(message: String) = new ValueCoercionViolation(message) {}

  private implicit val WorkflowIdType = fileBasedIdType[WorkflowPath]("WorkflowId", "Workflow's path and VersionId")
  private implicit val AgentIdType = fileBasedIdType[AgentPath]("AgentId", "Agent's path and VersionId")

  private def fileBasedIdType[P <: TypedPath: SomeType](name: String, description: String): ObjectType[QueryContext, FileBasedId[P]] = ObjectType(
    name,
    description,
    fields[QueryContext, FileBasedId[P]](
      Field("path", implicitly[SomeType[P]], resolve = _.value.path),
      Field("versionId", VersionIdType, resolve = _.value.versionId)))

  private val WorkflowJobType = ObjectType[QueryContext, WorkflowJob](
    "WorkflowJob",
    "Job",
    fields[QueryContext, WorkflowJob](
      Field("agentPath", AgentPathType, resolve = _.value.agentPath),
      Field("executablePath", ExecutablePathType, resolve = _.value.executablePath),
      Field("taskLimit", IntType, resolve = _.value.taskLimit)))

  private implicit val InstructionType = InterfaceType[QueryContext, Instruction](
    "Instruction",
    "Workflow instruction",
    fields[QueryContext, Instruction](
      Field("TYPE", StringType, resolve = ctx ⇒ Instructions.jsonCodec.typeName(ctx.value.getClass))))

  private implicit val InstructionTypes = List(
    ObjectType[QueryContext, AwaitOrder](
      "AwaitOrder",
      "Workflow instruction AwaitOrder",
      interfaces[QueryContext, AwaitOrder](InstructionType),
      fields[QueryContext, AwaitOrder]()),
    ObjectType[QueryContext, End](
      "End",
      "Workflow instruction End",
      interfaces[QueryContext, End](InstructionType),
      fields[QueryContext, End]()),
    ObjectType[QueryContext, ForkJoin](
      "ForkJoin",
      "Workflow instruction ForkJoin",
      interfaces[QueryContext, ForkJoin](InstructionType),
      fields[QueryContext, ForkJoin]()),
    ObjectType[QueryContext, Gap](
      "Gap",
      "Workflow instruction Gap",
      interfaces[QueryContext, Gap](InstructionType),
      fields[QueryContext, Gap]()),
    ObjectType[QueryContext, If](
      "If",
      "Workflow instruction If",
      interfaces[QueryContext, If](InstructionType),
      fields[QueryContext, If]()),
    ObjectType[QueryContext, IfNonZeroReturnCodeGoto](
      "IfNonZeroReturnCodeGoto",
      "Workflow instruction IfNonZeroReturnCodeGoto",
      interfaces[QueryContext, IfNonZeroReturnCodeGoto](InstructionType),
      fields[QueryContext, IfNonZeroReturnCodeGoto]()),
    ObjectType[QueryContext, Goto](
      "Goto",
      "Workflow instruction Goto",
      interfaces[QueryContext, Goto](InstructionType),
      fields[QueryContext, Goto]()),
    ObjectType[QueryContext, Execute.Anonymous](
      "Execute_Anonymous",
      "Workflow instruction Execute (named)",
      interfaces[QueryContext, Execute.Anonymous](InstructionType),
      fields[QueryContext, Execute.Anonymous](
        Field("job", WorkflowJobType, resolve = _.value.job))),
    ObjectType[QueryContext, Execute.Named](
      "Execute_Named",
      "Workflow instruction Execute (anonymous)",
      interfaces[QueryContext, Execute.Named](InstructionType),
      fields[QueryContext, Execute.Named](
        Field("name", WorkflowJobNameType, resolve = _.value.name))),
    ObjectType[QueryContext, Offer](
      "Offer",
      "Workflow instruction Offer",
      interfaces[QueryContext, Offer](InstructionType),
      fields[QueryContext, Offer]()))

  private implicit val PositionType = ListType(IntegerOrStringType)

  private implicit val WorkflowPositionType = ObjectType[QueryContext, WorkflowPosition](
    "WorkflowPosition",
    "WorkflowId and Position in Workflow",
    fields[QueryContext, WorkflowPosition](
      Field("workflowId", WorkflowIdType, resolve = _.value.workflowId),
      Field("position", PositionType, resolve = _.value.position.asSeq,
        description = "An array of a statement number (starting with 0) followed by nested positions"),
      Field("instruction", OptionType(InstructionType), resolve = ctx ⇒ {
        import ctx.ctx.executionContext
        ctx.ctx.idTo[Workflow](ctx.value.workflowId) map (_.toOption flatMap (_.instruction(ctx.value.position)))
      })))

  private implicit val StringStringMapType = ScalarType[Map[String, String]](
    "StringMap",
    "A map of String values",
    coerceOutput = (o, _) ⇒ ast.ObjectValue(o.map(kv ⇒ ast.ObjectField(kv._1, ast.StringValue(kv._2))).toVector),
    coerceUserInput = {
      case o: Map[_, _] if isStringStringMap(o) ⇒ Right(o.asInstanceOf[Map[String, String]])
      case _ ⇒ Left(StringCoercionViolation)
    },
    coerceInput = {
      case ast.ObjectValue(fields, _, _) ⇒
        val tuples = fields.map {
          case ast.ObjectField(k, v: ast.StringValue, _, _) ⇒ k → v.value
        }
        Right(tuples.toMap)
      case _ ⇒ Left(StringCoercionViolation)
    })

  private def isStringStringMap(map: Map[_, _]) =
    map forall {
      case (_: String, _: String) ⇒ true
      case _ ⇒ false
    }

  private implicit val OrderAttachedToType = ObjectType(
    "Order_AttachedTo",
    fields[Unit, Order.AttachedTo](
      Field("TYPE", StringType, resolve = _.value match {
        case _: Order.AttachedTo.Agent ⇒ "Agent"
        case _: Order.AttachedTo.Detachable ⇒ "Detachable"
      }),
      Field("agentId", OptionType(AgentIdType), resolve = _.value match {
        case o: Order.AttachedTo.AgentOrDetachable ⇒ Some(o.agentId)
        case _ ⇒ None
      })))

  private implicit val OutcomeType = InterfaceType(
    "Outcome",
    fields[QueryContext, Outcome](
      Field("TYPE", StringType, resolve = _.value.getClass.simpleScalaName)))

  private implicit val DisruptedOutcomeReasonType = InterfaceType(
    "DisruptedOutcomeReason",
    fields[QueryContext, Outcome.Disrupted.Reason](
      Field("TYPE", StringType, resolve = _.value.getClass.simpleScalaName),
      Field("message", OptionType(StringType), resolve = _.value match {
        case Outcome.Disrupted.Other(problem) ⇒ Some(problem.toString)
        case _ ⇒ None
      })))

  private val ReasonTypes = List(
    ObjectType(
      "JobSchedulerRestarted",
      interfaces[QueryContext, Outcome.Disrupted.JobSchedulerRestarted.type](DisruptedOutcomeReasonType),
      fields[QueryContext, Outcome.Disrupted.JobSchedulerRestarted.type]()),
    ObjectType(
      "DisruptedOutcomeOtherReason",
      interfaces[QueryContext, Outcome.Disrupted.Other](DisruptedOutcomeReasonType),
      fields[QueryContext, Outcome.Disrupted.Other](
        Field("message", StringType, resolve = _.value.message))))

  private val OutcomeSubtypes = List(
    ObjectType(
      "DisruptedOutcome",
      interfaces[QueryContext, Outcome.Disrupted](OutcomeType),
      fields[QueryContext, Outcome.Disrupted](
        Field("reason", DisruptedOutcomeReasonType, resolve = _.value.reason))),
    ObjectType(
      "FailedOutcome",
      interfaces[QueryContext, Outcome.Failed](OutcomeType),
      fields[QueryContext, Outcome.Failed](
        Field("returnCode", ReturnCodeType, resolve = _.value.returnCode))),
    ObjectType(
      "SucceededOutcome",
      interfaces[QueryContext, Outcome.Succeeded](OutcomeType),
      fields[QueryContext, Outcome.Succeeded](
        Field("returnCode", ReturnCodeType, resolve = _.value.returnCode))))

  private implicit val OrderStateType = InterfaceType(
    "OrderState",
    fields[QueryContext, Order.State](
      Field("TYPE", StringType, resolve = _.value.getClass.simpleScalaName)))

  private val OrderStateSubtypes = List(
    ObjectType(
      "FreshOrderState",
      interfaces[QueryContext, Order.Fresh](OrderStateType),
      fields[QueryContext, Order.Fresh](
        Field("scheduledAt", OptionType(LongType), resolve = _.value.scheduledAt map (_.toEpochMilli)))),
    ObjectType(
      "ReadyOrderState",
      interfaces[QueryContext, Order.Ready](OrderStateType),
      fields[QueryContext, Order.Ready]()),
    ObjectType(
      "InProcessOrderState",
      interfaces[QueryContext, Order.InProcess](OrderStateType),
      fields[QueryContext, Order.InProcess]()),
    ObjectType(
      "ProcessedOrderState",
      interfaces[QueryContext, Order.Processed](OrderStateType),
      fields[QueryContext, Order.Processed](
        Field("outcome", OutcomeType, resolve = _.value.outcome))),
    ObjectType(
      "StoppedOrderState",
      interfaces[QueryContext, Order.Stopped](OrderStateType),
      fields[QueryContext, Order.Stopped](
        Field("outcome", OutcomeType, resolve = _.value.outcome))),
    ObjectType(
      "ForkOrderState",
      interfaces[QueryContext, Order.Forked](OrderStateType),
      fields[QueryContext, Order.Forked](
        Field("childOrderIds", ListType(OrderIdType), resolve = _.value.childOrderIds))),
    ObjectType(
      "OfferedOrderState",
      interfaces[QueryContext, Order.Offered](OrderStateType),
      fields[QueryContext, Order.Offered]()),
    ObjectType(
      "AwaitingOrderState",
      interfaces[QueryContext, Order.Awaiting](OrderStateType),
      fields[QueryContext, Order.Awaiting](
        Field("offeredOrderId", OrderIdType  , resolve = _.value.offeredOrderId))),
    ObjectType(
      "FinishedOrderState",
      interfaces[QueryContext, Order.Finished](OrderStateType),
      fields[QueryContext, Order.Finished]()))

  private implicit val OrderType = ObjectType(
    "Order",
    fields[QueryContext, Order[Order.State]](
      Field("id", OrderIdType, resolve = _.value.id),
      Field("parent", OptionType(OrderIdType), resolve = _.value.parent,
        description = "A child Order has a parent Order"),
      Field("workflowPosition", WorkflowPositionType, resolve = _.value.workflowPosition,
        description = "The Order's current WorkflowId and Position in this Workflow"),
      Field("workflowPath", WorkflowPathType, resolve = _.value.workflowId.path),
      Field("attachedTo", OptionType(OrderAttachedToType), resolve = _.value.attachedTo,
        description = "Agent the Order is attached to"),
      Field("state", OrderStateType, resolve = _.value.state),
      Field("variables", OptionType(StringStringMapType), resolve = ctx ⇒ ctx.value.payload.variables.nonEmpty ? ctx.value.payload.variables)))

  private val OrderIdArg        = Argument("id", OrderIdType)
  private val OrderIdPatternArg = Argument("idPattern", OptionInputType(PatternType),
    description = "A regular expression matching the returned OrderIds")
  private val WorkflowPathArg   = Argument("workflowPath", OptionInputType(WorkflowPathType))
  private val LimitArg          = Argument("limit", OptionInputType(IntType),
    description = "Limits the resulting array")

  private val OrderQuery: ObjectType[QueryContext, Unit] = ObjectType(
    "Query",
    fields[QueryContext, Unit](
      Field("order", OptionType(OrderType),
        arguments = OrderIdArg :: Nil,
        resolve = ctx ⇒ ctx.ctx.order(ctx.arg(OrderIdArg)),
        description = "A single order identified by its OrderId"),
      Field("orders", ListType(OrderType),
        arguments = LimitArg :: WorkflowPathArg :: OrderIdPatternArg :: Nil,
        description = "The list of all orders, optionally filtered by some arguments",
        resolve = ctx ⇒ {
          import ctx.ctx.executionContext
          ctx.ctx.orders(QueryContext.OrderFilter(
            limit = ctx.arg(LimitArg) getOrElse Int.MaxValue,
            idPattern = ctx.arg(OrderIdPatternArg),
            workflowPath = ctx.arg(WorkflowPathArg)
          )) map (_.sortBy(_.id))
        })))

  val schema: Schema[QueryContext, Unit] = Schema(
    OrderQuery,
    additionalTypes = OrderStateSubtypes ::: OutcomeSubtypes ::: ReasonTypes ::: InstructionTypes)
}
