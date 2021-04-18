package lectures.part2afp

object PartialFunctions extends App {

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  println(aPartialFunction(2))
//  println(aPartialFunction(999))

  println(aPartialFunction.isDefinedAt(67))
  println(aPartialFunction.isDefinedAt(1))

  // lift
  val lifted: Int => Option[Int] = aPartialFunction.lift
  println(lifted(2))
  println(lifted(99))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }

  println(pfChain(2))
  println(pfChain(45))

  val aTotalFunction: Int => Int = {
    case 1 => 99
  }
  val aMappedList = List(1,2,3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }
  println(aMappedList)

  /**
   * Exercises
   * 1 - construct a PF instance yourself (anonymous class)
   * 2 - dumb chatbot as a PF
    */

  val chatbotPF = new PartialFunction[String, String] {
    override def apply(v1: String): String = v1 match {
      case "Hi" => "Hello"
      case "Bye" => "See you"
      case _ => "..."
    }

    override def isDefinedAt(x: String): Boolean = x.nonEmpty
  }

  scala.io.Source.stdin.getLines().map(chatbotPF).foreach(println)

}
