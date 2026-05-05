package js7.base.log.reader

object LogReaders:

  /** Hearbeat string for logs web service while recompressing and indexing.
    *
    * To entertain the reader while recompressing and indexing a large log without returning data
    * (due to a restrictive regex pattern), we send hearbeats.
    *
    * This line should not occur in a log file!
    */
  val LogHeartbeat = "🩶 JS7 LOG HEARTBEAT 🩶\n"
