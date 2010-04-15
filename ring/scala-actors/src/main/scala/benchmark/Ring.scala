package benchmark

import scala.actors._
import scala.actors.Actor._

object Ring {
  def main(args: Array[String]): Unit = {
    val node = startRing(10)
//    val node = startRing(args(0).toInt)
    node ! StartMessage
  }

  def startRing(n: Int): NodeActor = {
    val nodes = spawnNodes(n, startTimer())
    connectNodes(n, nodes)
    return nodes(0)
  }

  def startTimer(): TimerActor = {
    val timer = new TimerActor
    timer.start
    return timer
  }

  def spawnNodes(n: Int, timer: TimerActor): Array[NodeActor] = {
    println(" constructing nodes")
    val startConstructing = System.currentTimeMillis
    val nodes = new Array[NodeActor](n + 1)
    for (i <- 0 until n) {
      nodes(i) = new NodeActor(i, timer, null)
      nodes(i).start
    }
    val endConstructing = System.currentTimeMillis
    println("Took " + (endConstructing - startConstructing) + " ms to construct " + n + " nodes")
    return nodes
  }

  def connectNodes(n: Int, nodes: Array[NodeActor]) = {
    println("connecting nodes")
    nodes(n) = nodes(0)
    for (i <- 0 until n)
      nodes(i).connect(nodes(i + 1))
  }
}


case object StartMessage
case object StopMessage
case object CancelMessage
case class TokenMessage(id: Int, value: Int)


class NodeActor(id: Int, timer: TimerActor, var nextNode: NodeActor) extends Actor {
  val nodeId: Int = id

  def connect(node: NodeActor) = nextNode = node

  def act() {
    loop {
      react {
        case StartMessage => {
          log(" Starting messages ")
          timer ! StartMessage
          nextNode ! TokenMessage(nodeId, 0)
        }
        case StopMessage => {
          log(" Stopping ")
          nextNode ! StopMessage
          exit
        }
        case TokenMessage(id, value) if id == nodeId => {
          val nextValue = value + 1
          if (nextValue % 10000 == 0)
            log(" Around ring " + nextValue + " times ")

          if (nextValue == 1000000) {
            timer ! StopMessage
            timer ! CancelMessage
            nextNode ! StopMessage
            exit
          } else {
            nextNode ! TokenMessage(id, nextValue)
          }
        }
        case TokenMessage(id, value) => {
          nextNode ! TokenMessage(id, value)
        }
      }
    }
  }

  def log(msg: String) {
    // println(System.currentTimeMillis() + " " + nodeId + ": " + msg)

  }
}


class TimerActor() extends Actor {
  private var timing: Boolean = false
  private var startTime: Long = 0

  def act() {
    loop {
      react {
        case StartMessage if !timing => {
          startTime = System.currentTimeMillis()
          timing = true
        }
        case StopMessage if timing => {
          val end = System.currentTimeMillis()
          println(" Start = " + startTime + " Stop = " + end + " Elapsed = " + (end - startTime))
          timing = false
        }
        case CancelMessage => {
          exit
        }
      }
    }
  }
}
