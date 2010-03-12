package benchmark.akka

import java.lang._
import java.util.concurrent.CountDownLatch

import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.dispatch.Dispatchers

case object StopMessage

object ActorManager {
  val NR_REQUESTS = 1000000
  val latch = new CountDownLatch(3)
  def decrementLatch = latch.countDown

  def main(args: Array[String]) {
    import Actor.Sender.Self 
    System.setProperty("akka.config", "akka.conf")

    DownloadActor.start
    IndexActor.start
    WriteActor.start

    val start = System.currentTimeMillis
    for (i <- 1 until NR_REQUESTS) {
      val payload = "Requested " + i
      DownloadActor ! payload
    }
    DownloadActor ! StopMessage
    latch.await
    DownloadActor.stop
    IndexActor.stop
    WriteActor.stop
    println("Elapsed = " + (System.currentTimeMillis - start))
  }
}


object DownloadActor extends Actor {
  dispatcher = Dispatchers.newThreadBasedDispatcher(this)
  def receive = {
    case payload: String =>
      val newPayload = payload.replaceFirst("Requested ", "Downloaded ")
      IndexActor ! newPayload

    case StopMessage =>
      IndexActor ! StopMessage
      ActorManager.decrementLatch
  }
}

object IndexActor extends Actor {
  dispatcher = Dispatchers.newThreadBasedDispatcher(this)
  def receive = {
    case payload: String =>
      val newPayload = payload.replaceFirst("Downloaded ", "Indexed ")
      WriteActor ! newPayload

    case StopMessage => 
      WriteActor ! StopMessage
      ActorManager.decrementLatch
  }
}

object WriteActor extends Actor {
  dispatcher = Dispatchers.newThreadBasedDispatcher(this)
  def receive = {
    case payload: String =>
      payload.replaceFirst("Indexed ", "Wrote ")

    case StopMessage =>
      ActorManager.decrementLatch
  }
}