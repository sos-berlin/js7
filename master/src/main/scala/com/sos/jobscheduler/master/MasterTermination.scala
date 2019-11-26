package com.sos.jobscheduler.master

private[master] sealed trait MasterTermination

private[master] object MasterTermination
{
  final case object Terminate extends MasterTermination
  final case object Restart extends MasterTermination
}
