/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
*/
package benchmark.akka.actor;

import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.dispatch.{CompletableFuture, DefaultCompletableFuture}

object Colours extends Enumeration {
   val Blue = Value("blue")
   val Red = Value("red")
   val Yellow = Value("yellow")
}

import Colours.{Blue, Red, Yellow, Value => Colour}

case object Start
case class Meet(id: Int, c: Colour)
case class Pair(sameId: Boolean, colour: Colour)

class Creature(place: MeetingPlace, var colour: Colour) extends Actor {
  import ChameneosActor._
   val identity = System.identityHashCode(this)
   var sameCount = 0
   var count = 0
   start 
   place send "hello"
   
   def receive = {
     case Start => 
        try {
//          while (true) {
            val future: CompletableFuture = (place !! Meet(identity, colour)).getOrElse(throw new RuntimeException("Timed out"))
            future.await
            if (future.isCompleted) {
              val pair = future.result.get.asInstanceOf[Pair]              
              if (pair.sameId) sameCount += 1
              colour = pair.colour
              count +=1
            } else {
              println("FUTURE: " + future.exception)
            }
//          }
        } catch { case e => e.printStackTrace }
   }
   override def toString = String.valueOf(count) + " " + getNumber(sameCount)
}

class MeetingPlace(private var meetingsLeft: Int) extends Actor {
   import ChameneosActor._
   private var firstColour: Option[Colour] = None
   private var firstId = 0
   private var current: CompletableFuture = _
   start
   
   def receive = {
     case Meet(id, color) =>
       println("MEET 1")
       var newPair: CompletableFuture = null
       if (meetingsLeft == 0) {
         println("Stop: \t" + System.currentTimeMillis)
         throw new Exception("Finished")
       } else {
         println("MEET 2")
           if (firstColour.isEmpty) {
             println("MEET 3")
             firstColour = Some(color)
             firstId = id
             current = new DefaultCompletableFuture
           } else {
             println("MEET 4")
             val newColour = doCompliment(color, firstColour.get)
             println("MEET 5")
             current.completeWithResult(new Pair(id == firstId, newColour))
             println("MEET 6")
             firstColour = None
             println("MEET 7")
             meetingsLeft -= 1
             println("MEET 8")
           }
           println("MEET 9")
           newPair = current
       }
       println("MEET 10")
       reply(newPair)
       case msg => println("--------- ++++++ " + msg)
     }
}

object ChameneosActor {
   def doCompliment(c1: Colour, c2: Colour) = (c1, c2) match {
      case (Blue, Blue) => Blue
      case (Blue, Red) => Yellow
      case (Blue, Yellow) => Red
      case (Red, Blue) => Yellow
      case (Red, Red) => Red
      case (Red, Yellow) => Blue
      case (Yellow, Blue) => Red
      case (Yellow, Red) => Blue
      case (Yellow, Yellow) => Yellow
   }

   def run(n: Int, colours: Colour*) {
      val place = new MeetingPlace(n)

      colours.foreach { c => print(" " + c) }
      val creatures = colours.map(new Creature(place, _)).toArray

      println("Start: \t" + System.currentTimeMillis)
      creatures.foreach(_ send Start)

      Thread.sleep(60000)
      creatures.foreach(_.stop)
      place.stop      
      creatures.foreach(println)
      println(getNumber(creatures.foldLeft(0)(_ + _.count)))
      println()
   }

   def main(args: Array[String]) {
     System.setProperty("akka.config", "akka.conf")
      val n = if (args.isEmpty) 6000000 else Integer.parseInt(args(0))
      printColours()
      println()
      run(n, Blue, Red, Yellow, Red)
   }

   val Numbers = Array[String]("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

   def getNumber(n: Int) = String.valueOf(n).toList.map { ch => Numbers(Character.getNumericValue(ch)) } .mkString(" ")

   def printColours() {
      printColours(Blue, Blue)
      printColours(Blue, Red)
      printColours(Blue, Yellow)
      printColours(Red, Blue)
      printColours(Red, Red)
      printColours(Red, Yellow)
      printColours(Yellow, Blue)
      printColours(Yellow, Red)
      printColours(Yellow, Yellow)
   }

   def printColours(c1: Colour, c2: Colour) {
      println(c1+" + "+c2+" -> "+doCompliment(c1, c2))
   }
}
