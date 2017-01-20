package com.sos.scheduler.engine.taskserver.dotnet.api

/**
  * @author Joacim Zschimmer
  */
final case class TaskContext(
  spoolerLog: sos.spooler.Log,
  spoolerTask: sos.spooler.Task,
  spoolerJob: sos.spooler.Job,
  spooler: sos.spooler.Spooler)
