package com.sos.jobscheduler.common.utils

/**
  * @author Joacim Zschimmer
  */
object ByteUnits {

  /** Converts to bytes, kB, MB or GB */
  def toKBGB(size: Long): String =
    size match {
      case _ if size < 0 => s"${size}bytes"
      case _ if size == 0  => "0kB"
      case _ if size < 1000  => "<1kB"
      case _ if size < 1000000 => s"${size / 1000}kB"
      case _ if size < 1000000000 => s"${size / 1000000}MB"
      case _ => s"${size / 1000000000}GB"
    }

  def toMB(size: Long): String =
    size match {
      case _ if size < 0 => toKBGB(size)
      case _ if size == 0 => "0MB"
      case _ if size < 1000 * 1000 => "<1MB"
      case _ => toKBGB(size)
    }

  /** Converts to bytes, kB, MB or GB */
  def toKiBGiB(size: Long): String =
    size match {
      case _ if size < 0 => s"${size}bytes"
      case _ if size == 0  => "0KiB"
      case _ if size < 1024  => "<1KiB"
      case _ if size < 1024*1024 => s"${size / 1024}KiB"
      case _ if size < 1024*1024*1024 => s"${size / (1024*1024)}MiB"
      case _ => s"${size / (1024*1024*1024)}GiB"
    }
}
