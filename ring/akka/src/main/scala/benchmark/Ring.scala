package benchmark

import se.scalablesolutions.akka.actor.{Actor, ActorRef}
import se.scalablesolutions.akka.actor.Actor._
import se.scalablesolutions.akka.dispatch.Dispatchers
import se.scalablesolutions.akka.util.Logging

object Ring extends Logging {
  def main(args: Array[String]): Unit = {
    System.setProperty("akka.config", "akka.conf")
    //startRing(args(0).toInt) ! StartMessage
    startRing(10) !! StartMessage
  }

  def startRing(n: Int): ActorRef = {
    val nodes = spawnNodes(n)
    connectNodes(n, nodes)
    nodes(0)
  }

  def spawnNodes(n: Int): Array[ActorRef] = {
    println("Spawning actors")
    val startConstructing = System.currentTimeMillis
    val nodes = new Array[ActorRef](n + 1)
    for (i <- 0 until n) nodes(i) = actorOf(new NodeActor(i, null)).start
    val endConstructing = System.currentTimeMillis
    log.info("Took %s ms to construct %s nodes", (endConstructing - startConstructing), n)
    nodes
  }

  def connectNodes(n: Int, nodes: Array[ActorRef]) = {
    println("Connecting actors")
    nodes(n) = nodes(0)
    for (i <- 0 until n) nodes(i) ! Connect(nodes(i + 1))
  }
}

case object StartMessage
case object StopMessage
case object CancelMessage
case class Connect(node: ActorRef)
case class TokenMessage(id: Int, value: Int)

class NodeActor(id: Int, var nextNode: ActorRef) extends Actor {
  val nodeId: Int = id
  val timer = actorOf[TimerActor].start
  
  def receive = {
    case Connect(node) => 
      nextNode = node

    case StartMessage =>
      println("Start: \t" + System.currentTimeMillis)
      nextNode ! TokenMessage(nodeId, 0)

    case StopMessage =>
      if (nextNode.isRunning) nextNode ! StopMessage
      self.stop

    case TokenMessage(id, value) =>
      if (id == nodeId) {
        val nextValue = value + 1
        //if (nextValue % 10000 == 0) log.info("Around ring %s times", nextValue)
        if (nextValue == 1000000) {
          timer ! StopMessage
          timer ! CancelMessage
          println("Stop: \t" + System.currentTimeMillis)
          nextNode ! StopMessage
          self.stop
        } else nextNode ! TokenMessage(id, nextValue)        
      } else {
        nextNode ! TokenMessage(id, value)        
      }
  }
}

class TimerActor extends Actor {
  private var timing: Boolean = false
  private var startTime: Long = 0
  self.start

  def receive = {
    case StartMessage if !timing =>
      startTime = System.currentTimeMillis()
      timing = true

    case StopMessage if timing => 
      val end = System.currentTimeMillis
      println("Start = %s Stop = %s Elapsed = %s", startTime, end, (end - startTime))
      timing = false

    case CancelMessage => self.stop
  }
}
