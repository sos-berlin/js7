package js7.subagent.configuration

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

  def config = subagentConf.config
