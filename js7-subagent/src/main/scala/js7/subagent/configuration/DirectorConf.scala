package js7.subagent.configuration

import js7.base.io.https.HttpsConfig
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.journal.configuration.JournalConf

final case class DirectorConf(
  journalConf: JournalConf,
  httpsConfig: HttpsConfig,
  recouplingStreamReaderConf: RecouplingStreamReaderConf,
  subagentConf: SubagentConf):
  def config = subagentConf.config
