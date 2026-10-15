package js7.journal.memory

trait MemoryJournalMXBean:
  this: MemoryJournalMXBean.Bean =>

  def getEventQueueLength: Int

  def getEventQueueLimit: Int

  def getReleaseDuration: Double


private object MemoryJournalMXBean:

  final class Bean(val getEventQueueLimit: Int) extends MemoryJournalMXBean:
    var getEventQueueLength: Int = 0
    var getReleaseDuration: Double = Double.NaN
