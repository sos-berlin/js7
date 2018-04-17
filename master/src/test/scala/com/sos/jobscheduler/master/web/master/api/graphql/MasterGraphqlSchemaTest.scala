package com.sos.jobscheduler.master.web.master.api.graphql

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, VersionId}
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.instructions.{ForkJoin, Job}
import com.sos.jobscheduler.data.workflow.{Position, Workflow, WorkflowPath}
import com.sos.jobscheduler.master.web.master.api.graphql.MasterGraphqlSchemaTest._
import io.circe.Json
import org.scalatest.FreeSpec
import sangria.ast.Document
import sangria.execution.Executor
import sangria.macros._
import sangria.marshalling.circe._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class MasterGraphqlSchemaTest extends FreeSpec {

  "order" in {
    testGraphql(
      graphql"""{
        order(id: "1") {
          id
          workflowPath
        }
      }""",
      json"""{
        "data": {
          "order": {
            "id": "1",
            "workflowPath": "/A-WORKFLOW"
          }
        }
      }""")
  }

  "order with variable" in {
    testGraphql(
      graphql"""
        query Order($$id: OrderId!) {
          order(id: $$id) {
            id
            workflowPath
          }
        }""",
      """{
        "id": "1"
      }""".parseJson,
      json"""{
        "data": {
          "order": {
            "id": "1",
            "workflowPath": "/A-WORKFLOW"
          }
        }
      }""")
  }

  private val completeOrderGraphql = graphql"""{
      orders {
        id
        workflowPosition {
          workflowId {
            path
            versionId
          }
          position
          instruction {
            TYPE
            ... on Job {
              jobPath
              agentPath
            }
          }
        }
        attachedTo {
          TYPE
          agentId {
            path
            versionId
          }
        }
        state {
          TYPE
          ... on FreshOrderState {
            scheduledAt
          }
          ... on ProcessedOrderState {
            outcome {
              ...Outcome
            }
          }
          ... on StoppedOrderState {
            outcome {
              ...Outcome
            }
          }
          ... on JoinOrderState {
            joinOrderIds
          }
          ... on AwaitingOrderState {
            offeredOrderId
          }
        }
        variables
      }
    }

    fragment Outcome on Outcome {
      TYPE
      ... on SucceededOutcome {
        returnCode
      }
      ... on FailedOutcome {
        returnCode
      }
      ... on DisruptedOutcome {
        reason {
          TYPE
          message
        }
      }
    }"""

  "orders" in {
    testGraphql(completeOrderGraphql,
      json"""{
        "data": {
          "orders": [
            {
              "id": "1",
              "workflowPosition": {
                "workflowId": {
                  "path": "/A-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 0 ],
                "instruction": {
                  "TYPE": "Job",
                  "jobPath": "/JOB",
                  "agentPath": "/AGENT"
                }
              },
              "state": {
                "TYPE": "Fresh",
                "scheduledAt": 1523877753000
              }
            }, {
              "id": "10",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Awaiting",
                "offeredOrderId": "OFFERED"
              }
            }, {
              "id": "11",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Finished"
              }
            }, {
              "id": "2",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 1 ],
                "instruction": {
                  "TYPE": "ForkJoin"
                }
              },
              "state": {
                "TYPE": "Ready"
              }
            }, {
              "id": "3",
              "workflowPosition": {
                "workflowId": {
                  "path": "/A-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 1, "BRANCH", 0 ],
                "instruction": {
                  "TYPE": "Job",
                  "jobPath": "/BRANCH-0",
                  "agentPath": "/AGENT"
                }
              },
              "attachedTo": {
                "TYPE": "Agent",
                "agentId": {
                  "path": "/AGENT",
                  "versionId": "2"
                }
              },
              "state": {
                "TYPE": "InProcess"
              },
              "variables": {
                "KEY": "VALUE",
                "X": "XX"
              }
            }, {
              "id": "4",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Processed",
                "outcome": {
                  "TYPE": "Succeeded",
                  "returnCode": 7
                }
              }
            }, {
              "id": "5",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Processed",
                "outcome": {
                  "TYPE": "Failed",
                  "returnCode": 8
                }
              }
            }, {
              "id": "6",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Stopped",
                "outcome": {
                  "TYPE": "Disrupted",
                  "reason": {
                    "TYPE": "Other",
                    "message": "MESSAGE"
                  }
                }
              }
            }, {
              "id": "7",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Processed",
                "outcome": {
                  "TYPE": "Disrupted",
                  "reason": {
                    "TYPE": "JobSchedulerRestarted"
                  }
                }
              }
            }, {
              "id": "8",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Join",
                "joinOrderIds": [ "A", "B" ]
              }
            }, {
              "id": "9",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Offered"
              }
            }
          ]
        }
      }""")
  }

  if (sys.props contains "test.speed") "Speed test" - {
    val n = 100000
    def run: Json = Executor.execute(MasterGraphqlSchema.schema, completeOrderGraphql, bigContext(n)) await 99.s

    "Internal Json" in {
      for (_ ← 1 to 3) {
        val stopwatch = new Stopwatch
        run
        info(stopwatch.itemsPerSecondString(n, "orders(Json)"))
      }
    }

    "String JSON" in {
      val stopwatch = new Stopwatch
      run
      info(stopwatch.itemsPerSecondString(n, "orders(JSON)"))
    }
  }

  private def testGraphql(query: Document, expectedResult: Json): Unit =
    testGraphql(query, Json.obj(), expectedResult)

  private def testGraphql(query: Document, variables: Json, expectedResult: Json): Unit =
    assert(CompactPrinter.pretty(executeGraphql(query, variables) await 99.s).parseJson == expectedResult)

  private def executeGraphql(query: Document, variables: Json): Future[Json] =
    Executor.execute(MasterGraphqlSchema.schema, query, TestContext, variables = variables)
}

object MasterGraphqlSchemaTest
{
  private trait TestContext extends QueryContext {
    def executionContext = ExecutionContext.global

    protected val idToOrder = Vector[Order[Order.State]](
        Order(OrderId("1"), (WorkflowPath("/A-WORKFLOW") % "1") /: Position(0), Order.Fresh(Some(Timestamp.parse("2018-04-16T11:22:33Z")))),
        Order(OrderId("2"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(1), Order.Ready),
        Order(OrderId("3"), (WorkflowPath("/A-WORKFLOW") % "1") /: Position(1, "BRANCH", 0), Order.InProcess,
          Some(Order.AttachedTo.Agent(AgentPath("/AGENT") % "2")),
          Some(OrderId("PARENT")),
          Payload(Map("KEY" → "VALUE", "X" → "XX"))),
        Order(OrderId("4"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Processed(Outcome.Succeeded(ReturnCode(7)))),
        Order(OrderId("5"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Processed(Outcome.Failed(ReturnCode(8)))),
        Order(OrderId("6"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Stopped(Outcome.Disrupted("MESSAGE"))),
        Order(OrderId("7"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Processed(Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted))),
        Order(OrderId("8"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Join(OrderId("A") :: OrderId("B") :: Nil)),
        Order(OrderId("9"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Offered(Timestamp.parse("2018-04-16T11:22:33Z"))),
        Order(OrderId("10"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Awaiting(OrderId("OFFERED"))),
        Order(OrderId("11"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Finished)
      ).toKeyedMap(_.id)

    def order(orderId: OrderId) = Future.successful(idToOrder.get(orderId))

    def orders(filter: QueryContext.OrderFilter) = Future.successful(idToOrder.values.toVector take filter.limit)

    def idTo[A <: FileBased: FileBased.Companion](id: A#Id) =
      Future.successful(id match {
        case FileBasedId(_: WorkflowPath, VersionId("1")) ⇒
          Valid(Workflow.of(
            Job(JobPath("/JOB"), AgentPath("/AGENT")),
            ForkJoin(Vector(
              ForkJoin.Branch("BRANCH", Workflow.of(Job(JobPath("/BRANCH-0"), AgentPath("/AGENT"))))
            ))).asInstanceOf[A])
        case _ ⇒ Problem(s"No such ''$id'")
    })
  }
  private object TestContext extends TestContext

  private def bigContext(n: Int): QueryContext = new TestContext {
    private val order = Order(OrderId("?"), (WorkflowPath("/A-WORKFLOW") % "1") /: Position(0), Order.Fresh(Some(Timestamp.parse("2018-04-16T11:22:33Z"))))
    override val idToOrder = (1 to n).map(i ⇒ order.copy(id = OrderId(s"$i"))) toKeyedMap (_.id)
  }
}
