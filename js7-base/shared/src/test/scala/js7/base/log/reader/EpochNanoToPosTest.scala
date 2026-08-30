package js7.base.log.reader

import js7.base.io.OpaquePos
import js7.base.test.OurTestSuite
import js7.base.time.EpochNano

final class EpochNanoToPosTest extends OurTestSuite:

  "EpochNanoToPos" in:
    val nanoToPos = new EpochNanoToPos(initialSize = 1)
    assert(nanoToPos.isEmpty)
    assert(nanoToPos.length == 0)
    assert(nanoToPos.internalSize == 1)
    assert(nanoToPos.lastEpochNano == EpochNano.MinValue)
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(1)) == 0 -> OpaquePos(0))

    nanoToPos.add(EpochNano(10), OpaquePos(1000), 1111)
    nanoToPos.add(EpochNano(20), OpaquePos(2000), 2222)
    nanoToPos.add(EpochNano(30), OpaquePos(3000), 3333)
    assert(!nanoToPos.isEmpty)
    assert(nanoToPos.length == 3)
    assert(nanoToPos.internalSize == 16)

    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(0)) == 0 -> OpaquePos(0))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(9)) == 0 -> OpaquePos(0))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(10)) == 1111 -> OpaquePos(1000))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(11)) == 1111 -> OpaquePos(1000))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(20)) == 2222 -> OpaquePos(2000))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(21)) == 2222 -> OpaquePos(2000))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(30)) == 3333 -> OpaquePos(3000))
    assert(nanoToPos.epochNanoToChunkPosAndOpaquePos(EpochNano(31)) == 3333 -> OpaquePos(3000))

    assert(nanoToPos.posToChunkPosAndOpaquePos(0) == 0 -> OpaquePos(0))
    assert(nanoToPos.posToChunkPosAndOpaquePos(1111) == 1111 -> OpaquePos(1000))
    assert(nanoToPos.posToChunkPosAndOpaquePos(2222) == 2222 -> OpaquePos(2000))
    assert(nanoToPos.posToChunkPosAndOpaquePos(3333) == 3333 -> OpaquePos(3000))
    assert(nanoToPos.posToChunkPosAndOpaquePos(1009) == 0 -> OpaquePos(0))
    assert(nanoToPos.posToChunkPosAndOpaquePos(2009) == 1111 -> OpaquePos(1000))
    assert(nanoToPos.posToChunkPosAndOpaquePos(3009) == 2222 -> OpaquePos(2000))
    assert(nanoToPos.posToChunkPosAndOpaquePos(4009) == 3333 -> OpaquePos(3000))

    assert(nanoToPos.posToNextChunkPos(0) == Some(0))
    assert(nanoToPos.posToNextChunkPos(1111) == Some(1111))
    assert(nanoToPos.posToNextChunkPos(2222) == Some(2222))
    assert(nanoToPos.posToNextChunkPos(3333) == Some(3333))
    assert(nanoToPos.posToNextChunkPos(1009) == Some(1111))
    assert(nanoToPos.posToNextChunkPos(2009) == Some(2222))
    assert(nanoToPos.posToNextChunkPos(3009) == Some(3333))
    assert(nanoToPos.posToNextChunkPos(4009) == None)

    nanoToPos.shrink()
    assert(nanoToPos.length == 3)
    assert(nanoToPos.internalSize == 1 + 3)

  "EpochNanoToPos uses less memory for uncompressed log" in:
    val nanoToPos = new EpochNanoToPos(initialSize = 1)
    assert(nanoToPos.internalSize == 1)
    nanoToPos.add(EpochNano(1), OpaquePos(1000), 1000)
    nanoToPos.add(EpochNano(2), OpaquePos(2000), 2000)
    nanoToPos.add(EpochNano(3), OpaquePos(3000), 3000)
    assert(nanoToPos.internalSize == 16)
    assert(nanoToPos.isUsingNoMemoryForOpaquePos)
    assert(nanoToPos.posToChunkPosAndOpaquePos(1000) == 1000 -> OpaquePos(1000))

    nanoToPos.add(EpochNano(4), OpaquePos(4000), 4444)
    assert(!nanoToPos.isUsingNoMemoryForOpaquePos)
    assert(nanoToPos.posToChunkPosAndOpaquePos(4444) == 4444 -> OpaquePos(4000))
    assert(nanoToPos.posToChunkPosAndOpaquePos(9999) == 4444 -> OpaquePos(4000))
