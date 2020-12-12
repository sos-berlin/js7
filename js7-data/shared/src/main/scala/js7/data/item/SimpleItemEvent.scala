package js7.data.item

import js7.data.event.Event

trait SimpleItemEvent extends Event
{
  type Key <: SimpleItemId
}
