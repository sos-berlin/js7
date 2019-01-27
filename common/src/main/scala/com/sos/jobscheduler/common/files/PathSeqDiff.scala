package com.sos.jobscheduler.common.files

import java.nio.file.Path
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class PathSeqDiff(added: Seq[Path] = Nil, changed: Seq[Path] = Nil, deleted: Seq[Path] = Nil)
