/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Julien Gaugaz
   inspired by the version contributed by Yura Taras and modified by Isaac Gouy
*/
package benchmark.akka.actor;

import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.dispatch.Dispatchers

case class Exit(actor: Actor, reason: String)

object Chameneos {
  val pooled = Dispatchers.newExecutorBasedEventDrivenWorkStealingDispatcher("pooled-dispatcher")
  
  // messages
  case class Meet(from: Actor, colour:Colour)
  case class Change(colour: Colour)
  case class MeetingCount(count:Int)
  
  var start: Long = 0L
  var end: Long = 0L
  
  class Chameneo(var mall: Mall, var colour: Colour, cid:Int) extends Actor {
     dispatcher = pooled
     var meetings = 0
     start
     mall ! Meet(this, colour)

     def receive = {
       case Meet(from, otherColour) =>
         colour = complement(otherColour)
         meetings = meetings +1
         from ! Change(colour)
         mall ! Meet(this,colour)	

       case Change(newColour) =>
         colour = newColour
         meetings = meetings +1
         mall ! Meet(this,colour)	

       case Exit(_,_) =>
         colour = FADED
         sender.get ! MeetingCount(meetings)
         stop
     }

     def complement(otherColour: Colour): Colour = colour match {
       case RED => otherColour match {
         case RED => RED
         case YELLOW => BLUE
         case BLUE => YELLOW
         case FADED => FADED
       }
       case YELLOW => otherColour match {
         case RED => BLUE
         case YELLOW => YELLOW
         case BLUE => RED
         case FADED => FADED
       }
       case BLUE => otherColour match {
         case RED => YELLOW
         case YELLOW => RED
         case BLUE => BLUE
         case FADED => FADED
       }
       case FADED => FADED
     }

     override def toString() = cid+"("+colour+")"
   }

  class Mall(var n: Int, numChameneos: Int) extends Actor {
    var waitingChameneo: Option[Actor] = None
    var sumMeetings = 0
    var numFaded = 0

    start
    for (i <- 0 until numChameneos) new Chameneo(this, colours(i % 3), i)
    
    def receive = {
      case MeetingCount(i) =>
        numFaded += 1
        sumMeetings += i
        if (numFaded == numChameneos) {
          Chameneos.end = System.currentTimeMillis
          stop
        }
          
      case msg @ Meet(a, c) =>
        if (n > 0) {
          waitingChameneo match {
            case Some(chameneo) =>
              n -= 1
              chameneo ! msg 
              waitingChameneo = None
            case None => waitingChameneo = sender
          }
        } else {
          waitingChameneo.foreach(_ ! Exit(this, "normal"))
          sender.get ! Exit(this, "normal")
        }
    }
  }
  
  abstract class Colour
  case object RED extends Colour
  case object YELLOW extends Colour
  case object BLUE extends Colour
  case object FADED extends Colour
  
  val colours = Array[Colour](BLUE, RED, YELLOW)
  
  def main(args : Array[String]) : Unit = {
    System.setProperty("akka.config", "akka.conf")
    Chameneos.start = System.currentTimeMillis
    new Mall(1000000, 4)
    Thread.sleep(10000)
    println("Start: " + start)
    println("End: " + end)
    println("Elapsed: " + (end - start))
  }
}
