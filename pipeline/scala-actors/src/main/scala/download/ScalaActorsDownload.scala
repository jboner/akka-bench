package benchmark.scala

import java.lang._
import java.util.concurrent.CountDownLatch

import _root_.scala.actors._
import _root_.scala.actors.Actor._

object ActorManager {
  val NR_REQUESTS = 1000000

  val latch = new CountDownLatch(3)

  def decrementLatch = latch.countDown

  def main(args: Array[String]): Unit = {
    // start the actors
    DownloadActor.start
    IndexActor.start
    WriteActor.start
    // seed the download actor with requests
    val start = System.currentTimeMillis
    for (i <- 1 until NR_REQUESTS) {
      val payload = "Requested " + i
      DownloadActor ! payload
    }
    // ask them to stop
    DownloadActor ! StopMessage
    // wait for actors to stop
    latch.await
    println("elapsed = " + (System.currentTimeMillis - start))
  }
}

case class StopMessage()

object DownloadActor extends Actor {
  def act() {
    loop {
       receive {
        case payload: String => {
          val newPayload = payload.replaceFirst("Requested ", "Downloaded ")
          IndexActor ! newPayload
        }
        case StopMessage => {
          IndexActor ! StopMessage
          ActorManager.decrementLatch
          exit
        }
      }
    }
  }
}

object IndexActor extends Actor {
  def act() {
    loop {
      receive {
        case payload: String => {
          val newPayload = payload.replaceFirst("Downloaded ", "Indexed ")
          WriteActor ! newPayload
        }
        case StopMessage => {
          WriteActor ! StopMessage
          ActorManager.decrementLatch
          exit
        }
      }
    }
  }
}

object WriteActor extends Actor {
  def act() {
    loop {
      receive {
        case payload: String => {
          payload.replaceFirst("Indexed ", "Wrote ")
        }
        case StopMessage => {
          ActorManager.decrementLatch
          exit
        }
      }
    }
  }
}