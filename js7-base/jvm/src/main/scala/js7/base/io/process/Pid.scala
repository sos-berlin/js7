package js7.base.io.process

import js7.base.generic.GenericLong

final case class Pid(number: Long) extends GenericLong:

  def string = number.toString

  override def toString = s"PID:$number"


object Pid extends GenericLong.Companion[Pid]
