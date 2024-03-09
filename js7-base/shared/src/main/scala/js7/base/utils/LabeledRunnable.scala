package js7.base.utils

import js7.base.utils.ScalaUtils.*

final case class LabeledRunnable(label: String, runnable: Runnable) extends Runnable:

  def run() = runnable.run()

  override def toString = label


object LabeledRunnable:

  def apply(label: String)(callback: => Unit): LabeledRunnable =
    LabeledRunnable(label, () => callback)