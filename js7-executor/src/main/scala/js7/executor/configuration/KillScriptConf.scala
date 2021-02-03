package js7.executor.configuration

import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class KillScriptConf(killScript: ProcessKillScript, crashKillScriptFile: Path)
