package js7.common.files

import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class PathSeqDiff(added: Seq[Path] = Nil, changed: Seq[Path] = Nil, deleted: Seq[Path] = Nil):

  def isEmpty = added.isEmpty && changed.isEmpty && deleted.isEmpty
