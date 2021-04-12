package js7.executor.forjava.internal

import io.vavr.control.{Either => VEither}
import java.lang.reflect.Modifier.isPublic
import java.lang.reflect.{Constructor, InvocationTargetException}
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichEitherF}
import js7.base.utils.SetOnce
import js7.data.order.Outcome
import js7.data_for_java.vavr.VavrConverters._
import js7.executor.OrderProcess
import monix.eval.Task
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

private[internal] final class InternalJobAdapterHelper[J: ClassTag: TypeTag]
{
  private val checkedJobOnce = SetOnce[Checked[J]]  // SetOnce for late arriving Scheduler

  def callStart(jJobContext: JavaJobContext, call: J => Task[VEither[Problem, Void]]): Task[Checked[Unit]] =
    Task(instantiate(jJobContext))
      .flatMapT(jInternalJob =>
        call(jInternalJob)
          .map(_.toScala.rightAs(()))
          .materializeIntoChecked
          .tapEval(checked => Task {
            checkedJobOnce := checked.map(_ => jInternalJob)
            checked
          }))

  def callStop(call: J => Task[Unit]): Task[Unit] =
    Task.defer {
      call(checkedJobOnce.orThrow.orThrow)
    }

  def callProcessOrder(call: J => OrderProcess): OrderProcess =
    checkedJobOnce.checked.flatten match {
      case Left(problem) =>
        OrderProcess(Task.pure(Outcome.Failed.fromProblem(problem)))

      case Right(jInternalJob) =>
        call(jInternalJob)
    }

  private def instantiate(jJobContext: JavaJobContext): Checked[J] = {
    val cls = jJobContext.asScala.implementationClass
    if (!implicitClass[J].isAssignableFrom(cls))
      Left(Problem.pure(s"Class '${cls.getName}' must be a ${implicitClass[J].getName}"))
    else
      getConstructor(cls.asInstanceOf[Class[_ <: J]], jJobContext.getClass)
        .flatMap(construct(_, jJobContext))
  }

  private def getConstructor(clas: Class[_ <: J], jJobContextClass: Class[_ <: JavaJobContext])
  : Checked[Constructor[J]] =
    Checked.catchNonFatal {
      val constructors = clas
        .getConstructors.asInstanceOf[Array[Constructor[J]]]
        .filter(o => isPublic(o.getModifiers))
      constructors
        .find(_.getParameterTypes sameElements Array(jJobContextClass))
        .orElse(constructors.find(_.getParameterTypes.isEmpty))
        .toChecked(Problem.pure(
          s"Class '${clas.getName}' does not have an appropriate public constructor (empty or BlockingInternalJob.JobContext)"))
    }.flatten

  private def construct(constructor: Constructor[J], jobContext: JavaJobContext): Checked[J] =
    try
      Right(constructor.getParameterCount match {
        case 0 => constructor.newInstance()
        case 1 => constructor.newInstance(jobContext)
      })
    catch {
      case t @ (_: InvocationTargetException | _: ExceptionInInitializerError) =>
        Left(Problem.fromThrowable(Option(t.getCause) getOrElse t))
      case NonFatal(t) =>
        Problem.fromThrowable(t)
    }
}
