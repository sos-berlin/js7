package com.sos.scheduler.engine.data.order

/**
 * @author Joacim Zschimmer
 */
sealed trait OrderStateTransition

object OrderStateTransition {

  def ofCppInternalValue(internalValue: Long) = internalValue match {
    case Long.MaxValue ⇒
      KeepOrderStateTransition
    case i ⇒
      assert(i.toInt == i, s"OrderStateTransition($i)")
      ProceedingOrderStateTransition(i.toInt)
  }
}

/**
 * Order proceeds to another jobchain node.
 */
trait ProceedingOrderStateTransition extends OrderStateTransition {
  def resultValue: Int
}

object ProceedingOrderStateTransition {
  def apply(resultCode: Int) = resultCode match {
    case 0 ⇒ SuccessOrderStateTransition
    case i ⇒ ErrorOrderStateTransition(i)
  }
}

/**
 * Order proceeds to another jobchain node, used by attribute "next_state".
 */
case object SuccessOrderStateTransition extends OrderStateTransition {
  def resultValue = 0
}

/**
 * Order proceeds to another jobchain node, used by attribute "error_state".
 */
final case class ErrorOrderStateTransition(resultValue: Int) extends ProceedingOrderStateTransition

/**
 * Order step could not been completed and order stays in same jobchain node.
 */
case object KeepOrderStateTransition extends OrderStateTransition
