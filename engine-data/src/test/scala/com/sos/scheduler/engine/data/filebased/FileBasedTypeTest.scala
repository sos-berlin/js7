package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.lock.LockPath
import com.sos.scheduler.engine.data.order.OrderKey
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import com.sos.scheduler.engine.data.schedule.SchedulePath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class FileBasedTypeTest extends FreeSpec {
  "path" in {
    assert(FileBasedType.Folder.toPath("/a") == FolderPath("/a"))
    assert(FileBasedType.Job.toPath("/a") == JobPath("/a"))
    assert(FileBasedType.JobChain.toPath("/a") == JobChainPath("/a"))
    assert(FileBasedType.Lock.toPath("/a") == LockPath("/a"))
    assert(FileBasedType.ProcessClass.toPath("/a") == ProcessClassPath("/a"))
    assert(FileBasedType.Order.toPath("/a,1") == OrderKey("/a", "1"))
    assert(FileBasedType.Schedule.toPath("/a") == SchedulePath("/a"))
  }
}
