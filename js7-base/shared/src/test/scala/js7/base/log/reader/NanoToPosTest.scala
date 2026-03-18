package js7.base.log.reader

import js7.base.test.OurTestSuite
import js7.base.time.EpochNano

final class NanoToPosTest extends OurTestSuite:

  "EpochNanoToPos" in:
    val nanoToPos = new EpochNanoToPos(initialSize = 1)
    assert(nanoToPos.isEmpty)
    assert(nanoToPos.length == 0)
    assert(nanoToPos.internalSize == 1)
    assert(nanoToPos.toPos(EpochNano(1)) == 0)

    nanoToPos.add(EpochNano(10), 1000)
    nanoToPos.add(EpochNano(20), 2000)
    nanoToPos.add(EpochNano(30), 3000)
    assert(!nanoToPos.isEmpty)
    assert(nanoToPos.length == 3)
    assert(nanoToPos.internalSize == 16)

    assert(nanoToPos.toPos(EpochNano(0)) == 0)
    assert(nanoToPos.toPos(EpochNano(9)) == 0)
    assert(nanoToPos.toPos(EpochNano(10)) == 1000)
    assert(nanoToPos.toPos(EpochNano(11)) == 1000)
    assert(nanoToPos.toPos(EpochNano(20)) == 2000)
    assert(nanoToPos.toPos(EpochNano(21)) == 2000)
    assert(nanoToPos.toPos(EpochNano(30)) == 3000)
    assert(nanoToPos.toPos(EpochNano(31)) == 3000)

    nanoToPos.shrink()
    assert(nanoToPos.length == 3)
    assert(nanoToPos.internalSize == 1 + 3)
