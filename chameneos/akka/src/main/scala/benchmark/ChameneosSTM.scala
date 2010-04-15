/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
*/
package benchmark.akka.stm;

import se.scalablesolutions.akka.actor.{Transactor, Actor}
import se.scalablesolutions.akka.stm.Transaction.Local.atomic
import se.scalablesolutions.akka.stm.TransactionalRef

case object Start
case class Exit(actor: Actor, reason: String)

object Colours extends Enumeration {
   val Blue = Value("blue")
   val Red = Value("red")
   val Yellow = Value("yellow")
}

import Colours.{Blue, Red, Yellow, Value => Colour}

final class Creature(place: MeetingPlace, var colour: Colour) extends Actor {
   val ID = System.identityHashCode(this)
   var sameCount = 0
   var count = 0
   start 
   
   def receive = {
     case Start => 
        try {
          while (true) {
            println("MEET 1")
            val p = atomic { place.meet(ID, colour) }
            println("MEET 2")
            colour = p.colour
            println("MEET 3")
            if (p.sameId) sameCount += 1
            println("MEET 4")
            count +=1
            println("MEET 5")
          }
        } catch { case _: Exception => () }
   }
   override def toString = String.valueOf(count) + " " + ChameneosSTM.getNumber(sameCount)
}

final class MeetingPlace(val initNrMeetings: Int) {
  implicit val txFamily = "Meeting"
  
  var meetingsLeft: Option[TransactionalRef[Int]] = None
  var firstColour: Option[TransactionalRef[Colour]] = None
  var firstId: Option[TransactionalRef[Int]] = None
  var current: Option[TransactionalRef[Future]] = None

  def meet(id: Int, c: Colour): Pair = {
    if (meetingsLeft.isEmpty) {
      meetingsLeft = Some(TransactionalRef[Int]())
      try { meetingsLeft.get.swap(initNrMeetings) } catch { case e => e.printStackTrace }
      firstId = Some(TransactionalRef[Int]())
      firstId.get.swap(0)
      current = Some(TransactionalRef[Future]())
    } 
    if (meetingsLeft.get.get == 0) {
      println("\nStop: \t" + System.currentTimeMillis)
      throw new Exception("Finished")
    } else {
      //atomic {
        try { 
        if (firstColour.isEmpty || firstColour.get.isEmpty) {
          firstColour = Some(TransactionalRef[Colour]())
          firstColour.get.swap(c)
          firstId.get.swap(id)
          current.get.swap(new Future)
        } else {
          val compliment = ChameneosSTM.doCompliment(c, firstColour.get.get.get)
          current.get.get.get.setItem(new Pair(id == firstId.get.get.get, compliment))
          firstColour = None
          meetingsLeft.get.swap(meetingsLeft.get.getOrElse(throw new Error("eee")) - 1)
        }
        } catch { case e => e.printStackTrace }
        current.get.get.get
      //}
    }.getItem
  }
}

final class Future {
   @volatile var p: Pair = _

   def getItem() = {
      while (p == null)
         Thread.`yield`()
      p
   }

   def setItem(_p: Pair) {
      this.p = _p
   }
}

final case class Pair(sameId: Boolean, colour: Colour)

object ChameneosSTM {
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
      colours.foreach(c => print(" " + c))
      val creatures = colours.map(new Creature(place, _)).toArray

      println("\nStart: \t" + System.currentTimeMillis)
      implicit val txName = "ChameneosSTM"
      creatures.foreach(_ ! Start)

      Thread.sleep(1000 * 20)
      creatures.foreach(_.stop)
      creatures.foreach(println)
      println(getNumber(creatures.foldLeft(0){_ + _.count}))
   }

   def main(args: Array[String]) {
      System.setProperty("akka.config", "akka.conf")
      val n = if (args.isEmpty) 6000000 else Integer.parseInt(args(0))
      printColours()
      println()
      run(n, Blue, Red, Yellow, Red)
//      run(n, Blue, Red, Yellow, Red, Yellow, Blue, Red, Yellow, Red, Blue)
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
