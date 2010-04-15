package benchmark

import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.dispatch.Dispatchers
import se.scalablesolutions.akka.util.Logging

object Ring extends Logging {
  def main(args: Array[String]): Unit = {
    System.setProperty("akka.config", "akka.conf")
    //startRing(args(0).toInt) ! StartMessage
    startRing(10) !! StartMessage
  }

  def startRing(n: Int): NodeActor = {
    val nodes = spawnNodes(n)
    connectNodes(n, nodes)
    nodes(0)
  }

  def spawnNodes(n: Int): Array[NodeActor] = {
    println("Spawning actors")
    val startConstructing = System.currentTimeMillis
    val nodes = new Array[NodeActor](n + 1)
    for (i <- 0 until n) {
      nodes(i) = new NodeActor(i, null)
      nodes(i).start
    }
    val endConstructing = System.currentTimeMillis
    log.info("Took %s ms to construct %s nodes", (endConstructing - startConstructing), n)
    nodes
  }

  def connectNodes(n: Int, nodes: Array[NodeActor]) = {
    println("Connecting actors")
    nodes(n) = nodes(0)
    for (i <- 0 until n) nodes(i).connect(nodes(i + 1))
  }
}

case object StartMessage
case object StopMessage
case object CancelMessage
case class TokenMessage(id: Int, value: Int)

class NodeActor(id: Int, var nextNode: NodeActor) extends Actor {
  val nodeId: Int = id

  def connect(node: NodeActor) = nextNode = node

  def receive = {
    case StartMessage =>
      println("Start: \t" + System.currentTimeMillis)
//      TimerActor ! StartMessage
      nextNode ! TokenMessage(nodeId, 0)

    case StopMessage =>
      if (nextNode.isRunning) nextNode ! StopMessage
      stop

    case TokenMessage(id, value) =>
      if (id == nodeId) {
        val nextValue = value + 1
        //if (nextValue % 10000 == 0) log.info("Around ring %s times", nextValue)
        if (nextValue == 1000000) {
          TimerActor ! StopMessage
          TimerActor ! CancelMessage
          println("Stop: \t" + System.currentTimeMillis)
          nextNode ! StopMessage
          stop
        } else nextNode ! TokenMessage(id, nextValue)        
      } else {
        nextNode ! TokenMessage(id, value)        
      }
  }
}

object TimerActor extends Actor {
  private var timing: Boolean = false
  private var startTime: Long = 0
  start

  def receive = {
    case StartMessage if !timing =>
      startTime = System.currentTimeMillis()
      timing = true

    case StopMessage if timing => 
      val end = System.currentTimeMillis
      println("Start = %s Stop = %s Elapsed = %s", startTime, end, (end - startTime))
      timing = false

    case CancelMessage => stop
  }
}
