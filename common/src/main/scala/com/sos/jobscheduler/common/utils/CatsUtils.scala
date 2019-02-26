package com.sos.jobscheduler.common.utils

import cats.effect.{Resource, SyncIO}
import java.io.{ByteArrayInputStream, InputStream}
import java.util.Base64

/**
  * @author Joacim Zschimmer
  */
object CatsUtils
{
  def bytesToInputStreamResource(bytes: collection.Seq[Byte]): Resource[SyncIO, InputStream] =
    bytesToInputStreamResource(bytes.toArray)

  def bytesToInputStreamResource(bytes: Array[Byte]): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO { new ByteArrayInputStream(bytes): InputStream })

  def base64ToStreamResource(base64: String): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO[InputStream] {
      try new ByteArrayInputStream(Base64.getMimeDecoder.decode(base64))
      catch { case e: IllegalArgumentException =>
        throw new IllegalArgumentException(s"Error in Base64 encoded data: ${e.getMessage}", e)
      }
    })
}
