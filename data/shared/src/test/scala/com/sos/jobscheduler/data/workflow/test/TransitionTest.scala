package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TransitionTest extends FreeSpec {

  "nodes, fromNodeIds, toNodeIds" in {
    assert(a.nodes == List(Bx, Cx, By, Cy))
    assert(a.fromNodeIds == List(A.id))
    assert(a.toNodeIds == Vector(Bx.id, By.id))

    assert(bx.nodes.isEmpty)
    assert(bx.fromNodeIds == List(Bx.id))
    assert(bx.toNodeIds == Vector(Cx.id))

    assert(by.nodes.isEmpty)
    assert(by.fromNodeIds == List(By.id))
    assert(by.toNodeIds == Vector(Cy.id))

    assert(c.nodes.isEmpty)
    assert(c.fromNodeIds == List(A.id, Cx.id, Cy.id))
    assert(c.toNodeIds == Vector(D.id))

    assert(d.nodes == Vector(Ex, Fx, Ey, Fy))
    assert(d.fromNodeIds == List(D.id))
    assert(d.toNodeIds == Vector(Ex.id, Ey.id))

    assert(ex.nodes.isEmpty)
    assert(ex.fromNodeIds == List(Ex.id))
    assert(ex.toNodeIds == Vector(Fx.id))

    assert(ey.nodes.isEmpty)
    assert(ey.fromNodeIds == List(Ey.id))
    assert(ey.toNodeIds == Vector(Fy.id))

    assert(f.nodes.isEmpty)
    assert(f.fromNodeIds == List(D.id, Fx.id, Fy.id))
    assert(f.toNodeIds == Vector(G.id))

    assert(g.nodes.isEmpty)
    assert(g.fromNodeIds == List(G.id))
    assert(g.toNodeIds == Vector(END.id))
  }
}
