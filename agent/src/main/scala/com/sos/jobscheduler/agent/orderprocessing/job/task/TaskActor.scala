//package com.sos.jobscheduler.agent.orderprocessing.job.task
//
//import akka.actor.{Actor, ActorRef, FSM, Props}
//import com.sos.jobscheduler.data.engine2.order.Order
//import com.sos.jobscheduler.agent.orderprocessing.job.JobConfiguration
//import com.sos.jobscheduler.agent.orderprocessing.job.task.TaskActor._
//import com.sos.jobscheduler.agent.task.AgentTaskFactory
//import com.sos.jobscheduler.base.process.ProcessSignal
//import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
//import com.sos.jobscheduler.data.job.TaskId
//import scala.concurrent.blocking
//import scala.util.Try
//
///**
//  * @author Joacim Zschimmer
//  */
//private final class TaskActor private(jobConfiguration: JobConfiguration, taskId: TaskId, newTask: AgentTaskFactory)
//extends Actor with FSM[State, Data] {
//
//  import context.dispatcher
//
//  private val taskRunnerOnce = new SetOnce[TaskRunner]
//  private var killed = false
//
//  startWith(AwaitingOrder, NoData)
//
//  override def postStop() = {
//    blocking { // TaskServer.close blocks
//      for (t ← taskRunnerOnce) t.terminate()
//    }
//    super.postStop()
//  }
//
//  when(AwaitingOrder) {
//    case Event(AddOrder(order), NoData) ⇒
//      val taskRunner = taskRunnerOnce getOrUpdate {
//        new TaskRunner(jobConfiguration, taskId, newTask)
//      }
//      taskRunner.processOrder(order)
//        .onComplete {
//          o ⇒ self ! OrderProcessed(o)
//        }
//      goto(OrderProcessing) using OrderProcessingData(sender())
//
//    case Event(Kill(signal), _) ⇒
//      kill(signal)
//      sender() ! Killed
//      if (killed) {
//        terminate()
//        stop()
//      } else
//        stay()
//  }
//
//  when(OrderProcessing) {
//    case Event(response: OrderProcessed, OrderProcessingData(orderSender)) ⇒
//      orderSender ! response
//      if (true || killed) {
//        terminate()
//        stop()
//      } else
//        goto(AwaitingOrder)
//
//    case Event(Kill(signal), _) ⇒
//      kill(signal)
//      sender() ! Killed
//      stay()
//  }
//
//  private def kill(signal: ProcessSignal): Unit = {
//    for (t ← taskRunnerOnce) {
//      t.kill(signal)
//      killed |= t.killed
//    }
//  }
//
//  private def terminate(): Unit = {
//    blocking {
//      for (o ← taskRunnerOnce) o.terminate()
//    }
//  }
//}
//
//private object TaskActor {
//  sealed trait Command
//  final case class AddOrder(order: Order) extends Command
//  final case class Kill(signal: ProcessSignal) extends Command
//
//  sealed trait Response
//  object Killed extends Response
//
//  sealed trait State
//  private case object AwaitingOrder extends State
//  private case object OrderProcessing extends State
//
//  sealed trait Data
//  private case object NoData extends Data
//  private case class OrderProcessingData(orderSender: ActorRef) extends Data
//
//  def props(jobConfiguration: JobConfiguration, taskId: TaskId, newTask: AgentTaskFactory): Props =
//    Props { new TaskActor(jobConfiguration, taskId, newTask) }
//}
