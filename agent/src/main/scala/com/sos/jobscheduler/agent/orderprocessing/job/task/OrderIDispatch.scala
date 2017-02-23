package com.sos.jobscheduler.agent.orderprocessing.job.task

import com.sos.jobscheduler.minicom.idispatch.{AnnotatedInvocable, InvocableIDispatch}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class OrderIDispatch(initialVariables: Map[String, String]) extends InvocableIDispatch with AnnotatedInvocable {

  private var _variables = mutable.Map[String, String]() ++ initialVariables

  def variables: Map[String, String] = _variables.toMap

  def mergeVariables(o: Map[String, String]): Unit = {
    _variables ++= o
  }
}
