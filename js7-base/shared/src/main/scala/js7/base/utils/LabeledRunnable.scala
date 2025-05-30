package js7.base.utils

final case class LabeledRunnable(label: String, runnable: Runnable) extends Runnable:

  def run(): Unit = runnable.run()

  override def toString: String = label


object LabeledRunnable:

  def apply(label: String)(callback: => Unit): LabeledRunnable =
    LabeledRunnable(label, () => callback)




// ⭐️ This can rename the thread, maybe for testing:
//
//private trait LabeledRunnable extends Runnable:
//  def label: String
//
//  override def toString = label
//
//private object LabeledRunnable:
//  def apply(label: String)(body: => Unit) =
//    val label_ = label
//    new LabeledRunnable:
//      def label = label_
//      def run() = body
//
//  private trait RelabelsThread extends ThreadPoolExecutor:
//    private var restoreName: () => Unit = null
//
//    override def beforeExecute(thread: Thread, runnable: Runnable) =
//      super.beforeExecute(thread, runnable)
//      runnable match
//        case runnable: LabeledRunnable =>
//          val originalName = thread.getName
//          restoreName = () => thread.setName(originalName)
//          thread.setName(runnable.label)
//        case _ =>
//
//    override def afterExecute(runnable: Runnable, throwable: Throwable) =
//      runnable match
//        case _: LabeledRunnable =>
//          if restoreName != null then
//            restoreName()
//            restoreName = null
//        case _ =>
//      super.afterExecute(runnable, throwable)
