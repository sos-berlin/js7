package js7.common.scalautil

import java.util.concurrent.ThreadPoolExecutor

// NOT USED !!!
/**
  * @author Joacim Zschimmer
  */
private trait NamedRunnable extends Runnable:
  def name: String

  override def toString = name

private object NamedRunnable:
  def apply(name: String)(body: => Unit) =
    val name_ = name
    new NamedRunnable:
      def name = name_
      def run() = body

  private trait RenamesThread extends ThreadPoolExecutor:
    private var restoreName: () => Unit = null

    override def beforeExecute(thread: Thread, runnable: Runnable) =
      super.beforeExecute(thread, runnable)
      runnable match
        case runnable: NamedRunnable =>
          val originalName = thread.getName
          restoreName = () => thread.setName(originalName)
          thread.setName(runnable.name)
        case _ =>

    override def afterExecute(runnable: Runnable, throwable: Throwable) =
      runnable match
        case _: NamedRunnable =>
          if restoreName != null then
            restoreName()
            restoreName = null
        case _ =>
      super.afterExecute(runnable, throwable)
