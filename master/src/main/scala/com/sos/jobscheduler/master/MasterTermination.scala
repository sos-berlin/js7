package com.sos.jobscheduler.master

private[master] sealed trait MasterTermination

private[master] object MasterTermination
{
  final case class Terminate(restart: Boolean = false) extends MasterTermination

  final case object Restart extends MasterTermination
}
