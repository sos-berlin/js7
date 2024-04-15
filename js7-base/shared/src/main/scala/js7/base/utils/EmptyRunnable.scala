package js7.base.utils

object EmptyRunnable extends Runnable:
  def run(): Unit = {}

val emptyRunnable: Runnable = EmptyRunnable
