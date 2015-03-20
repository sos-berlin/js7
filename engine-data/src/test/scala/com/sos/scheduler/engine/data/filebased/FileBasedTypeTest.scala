package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.lock.LockPath
import com.sos.scheduler.engine.data.order.OrderKey
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import com.sos.scheduler.engine.data.schedule.SchedulePath
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class FileBasedTypeTest extends FreeSpec {
  "path" in {
    assert(FileBasedType.folder.toPath("/a") == FolderPath("/a"))
    assert(FileBasedType.job.toPath("/a") == JobPath("/a"))
    assert(FileBasedType.jobChain.toPath("/a") == JobChainPath("/a"))
    assert(FileBasedType.lock.toPath("/a") == LockPath("/a"))
    assert(FileBasedType.processClass.toPath("/a") == ProcessClassPath("/a"))
    assert(FileBasedType.order.toPath("/a,1") == OrderKey("/a", "1"))
    assert(FileBasedType.schedule.toPath("/a") == SchedulePath("/a"))
  }
}
