package com.sos.jobscheduler.agent.data

import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class KillScriptConf(killScript: ProcessKillScript, crashKillScriptFile: Path)
