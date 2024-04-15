package js7.subagent.configuration

import com.typesafe.config.Config
import js7.base.io.https.HttpsConfig
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.journal.configuration.JournalConf
import js7.subagent.director.RemoteSubagentDriver

final case class DirectorConf(
  journalConf: JournalConf,
  httpsConfig: HttpsConfig,
  recouplingStreamReaderConf: RecouplingStreamReaderConf,
  subagentDriverConf: RemoteSubagentDriver.Conf,
  subagentConf: SubagentConf):

  def config: Config = subagentConf.config
