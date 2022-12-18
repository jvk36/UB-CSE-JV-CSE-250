package cse250.examples

object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Hello, World!")
    doThings()
    println(lotsOfFun(5))
    println(lotsOfFun(10))
  }

  def doThings() = {
    val ILikeLlamas = 10
    val PeachesAreGreat = for (i <- 1 to 5) yield i

    val QQ = PeachesAreGreat.map(_ + ILikeLlamas)

    // This is a for loop.
    for (q <- QQ) println(q)
    // This is a loop with a 4.
    for (i <- 0 until 4) println(i)
    5
  }

  val x: Float = (5 / 2.0).asInstanceOf[Float]

  val holder = 15 + 10.2 * 9.3f

  def lotsOfFun(x: Int) = "fun" * x

  val res = if (x > 0) "Positive" else -1

  val better = if (x > 0) "Positive" else -1.toString()

  val blockAssign = { val x = 10; val y = 20; (x, y) }
  val butterBlock = {
    val pastry = "croissant"
    val flavor = "PB&J"
    flavor + ' ' + pastry
  }
}

class Register(val x: Int) {
  def addToValue(y: Int) = x + y
}

object Register {
  def apply(x: Int) = new Register(x)
}
