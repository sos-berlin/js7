package js7.base.eventbus

trait EventPublisher[Event]
{
  def publish(event: Event): Unit
}
