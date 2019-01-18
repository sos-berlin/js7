package com.sos.jobscheduler.common.scalautil

import cats.effect.{Resource, SyncIO}
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
object JavaSyncResources
{
  def fileAsResource(path: Path): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO[InputStream] {
      new BufferedInputStream(new FileInputStream(path.toFile))
    })
}
