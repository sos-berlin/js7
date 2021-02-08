package js7.executor.forjava.internal

import java.lang.reflect.Modifier.isPublic
import java.lang.reflect.{Constructor, InvocationTargetException}
import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.base.utils.SetOnce
import js7.executor.internal.InternalJob.{JobContext, OrderContext, OrderProcess}
import monix.eval.Task
import monix.execution.Scheduler
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

private[internal] final class InternalJobAdapterHelper[J: ClassTag: TypeTag]
{
  private val checkedJobOnce = SetOnce[Checked[J]]  // SetOnce for late arriving Scheduler

  private[internal] def checkedJob: Checked[J] =
    checkedJobOnce.checked.flatten

  def start(jobContext: JobContext, call: J => Task[Unit]): Task[Checked[Unit]] =
    Task
      .deferAction(scheduler =>
        Task(instantiate(jobContext, scheduler)))
      .flatMapT { jInternalJob =>
        val task = call(jInternalJob)
          .materialize
          .map(Checked.fromTry)
          .tapEval(checked => Task {
            checkedJobOnce := checked.map(_ => jInternalJob)
            checked
          })
        task
      }

  def processOrder(
    context: OrderContext,
    call: (J, JOrderContext) => OrderProcess)
  : OrderProcess = {
    checkedJobOnce.checked.flatten match {
      case Left(problem) => OrderProcess(Task.pure(Left(problem)))
      case Right(jInternalJob) => call(jInternalJob, JOrderContext(context))
    }
  }

  private def instantiate(jobContext: JobContext, scheduler: Scheduler): Checked[J] = {
    if (!implicitClass[J].isAssignableFrom(jobContext.implementationClass))
      Left(Problem.pure(s"Class '${jobContext.implementationClass.getName}' must be a ${implicitClass[J].getName}"))
    else
      getConstructor(jobContext.implementationClass.asInstanceOf[Class[_ <: J]])
        .flatMap(construct(_, JJobContext(jobContext, scheduler)))
  }

  private def getConstructor(clas: Class[_ <: J]): Checked[Constructor[J]] =
    Checked.catchNonFatal {
      val constructors = clas
        .getConstructors.asInstanceOf[Array[Constructor[J]]]
        .filter(o => isPublic(o.getModifiers))
      constructors
        .find(_.getParameterTypes sameElements Array(classOf[JJobContext]))
        .orElse(constructors.find(_.getParameterTypes.isEmpty))
        .toChecked(Problem.pure(
          s"Class '${clas.getName}' does not have an appropriate constructor (empty or InternalJob.Context)"))
    }.flatten

  private def construct(constructor: Constructor[J], jobContext: JJobContext): Checked[J] =
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
