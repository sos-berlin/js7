package js7.base.eventbus

trait EventPublisher[E]:

  def publish(event: E): Unit

  def narrowPublisher[E1 <: E]: EventPublisher[E1] =
    this.asInstanceOf[EventPublisher[E1]]
