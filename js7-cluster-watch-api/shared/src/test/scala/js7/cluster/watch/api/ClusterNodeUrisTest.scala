package js7.cluster.watch.api

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.data.event.{EventRequest, JournalPosition}
import js7.data.order.OrderEvent

/**
  * @author Joacim Zschimmer
  */
final class ClusterNodeUrisTest extends OurTestSuite:
  private val uris = ClusterNodeUris(Uri("https://example.com/controller"))

  "command" in:
    assert(uris.command == Uri("https://example.com/controller/api/cluster/command"))

  "events" in:
    assert(uris.events(EventRequest.singleClass[OrderEvent](after = 7, timeout = Some(1230.ms))) ==
      Uri("https://example.com/controller/api/event?return=OrderEvent&delay=0&timeout=1.23&after=7"))
    assert(uris.events(EventRequest.singleClass[OrderEvent](after = 7, timeout = Some(1230.ms), limit = 333)) ==
      Uri("https://example.com/controller/api/event?return=OrderEvent&delay=0&timeout=1.23&limit=333&after=7"))

    // return EventId only
    assert(uris.eventIds(timeout = Some(1.s)) ==
      Uri("https://example.com/controller/api/event?onlyAcks=true&timeout=1"))

  "clusterState" in:
    assert(uris.clusterState == Uri("https://example.com/controller/api/cluster"))

  "journal" in:
    assert(uris.journal(JournalPosition(100, 111), timeout = Some(50.s)) ==
      Uri("https://example.com/controller/api/journal?timeout=50&file=100&position=111"))
    assert(uris.journal(JournalPosition(100, position = 111), heartbeat = Some(22.s), markEOF = true, returnAck = true) ==
      Uri("https://example.com/controller/api/journal?return=ack&heartbeat=22&markEOF=true&file=100&position=111"))
