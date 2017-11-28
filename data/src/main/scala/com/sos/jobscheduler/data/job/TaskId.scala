package com.sos.jobscheduler.data.job

import com.sos.jobscheduler.base.generic.GenericInt

final case class TaskId(number: Int) extends GenericInt {

  override def toString = s"TaskId $number"

  def +(n: Int) = TaskId(number + n)

  def -(n: Int) = TaskId(number - n)

  def string = number.toString
}

object TaskId extends GenericInt.Companion[TaskId]
