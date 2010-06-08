package benchmark.akka

import java.lang._
import java.util.concurrent.CountDownLatch

import se.scalablesolutions.akka.actor.{Actor, ActorRef}
import se.scalablesolutions.akka.actor.Actor._
import se.scalablesolutions.akka.dispatch.Dispatchers

case object StopMessage

object ActorManager {
  val NR_REQUESTS = 1000000
  val latch = new CountDownLatch(3)
  def decrementLatch = latch.countDown

  def main(args: Array[String]) {
    System.setProperty("akka.config", "akka.conf")

    val write = actorOf[WriteActor].start
    val index = actorOf(new IndexActor(write)).start
    val download = actorOf(new DownloadActor(index)).start

    val start = System.currentTimeMillis
    for (i <- 1 until NR_REQUESTS) download ! ("Requested " + i)
    download ! StopMessage
    latch.await
    download.stop
    index.stop
    write.stop
    println("Elapsed = " + (System.currentTimeMillis - start))
  }
}


class DownloadActor(index: ActorRef) extends Actor {
  self.dispatcher = Dispatchers.newThreadBasedDispatcher(self)
  def receive = {
    case payload: String =>
      index ! payload.replaceFirst("Requested ", "Downloaded ")

    case StopMessage =>
      index ! StopMessage
      ActorManager.decrementLatch
  }
}

class IndexActor(write: ActorRef) extends Actor {
  self.dispatcher = Dispatchers.newThreadBasedDispatcher(self)
  def receive = {
    case payload: String =>
      write ! payload.replaceFirst("Downloaded ", "Indexed ")

    case StopMessage => 
      write ! StopMessage
      ActorManager.decrementLatch
  }
}

class WriteActor extends Actor {
  self.dispatcher = Dispatchers.newThreadBasedDispatcher(self)
  def receive = {
    case payload: String =>
      payload.replaceFirst("Indexed ", "Wrote ")

    case StopMessage =>
      ActorManager.decrementLatch
  }
}
