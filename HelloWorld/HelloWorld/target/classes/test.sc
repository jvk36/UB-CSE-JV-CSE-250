val x: Float = (5 / 2.0).asInstanceOf[Float]
val res = if (x > 0) "Positive" else -1
val better = if (x > 0) "Positive" else (-1).toString()

val blockAssign = { val x = 10; val y = 20; (x, y) }

val butterBlock = {
  val pastry = "croissant"
  val flavor = "PB&J"
  flavor + ' ' + pastry
}

class Register(val x: Int) {
  def addToValue(y: Int) = x + y
}

object Register {
  def apply(x: Int) = new Register(x)
}

val a = Register(10)
println(a.addToValue(20))

val list = List(1, 3, 4, 5) // list must be sorted before searching
list.search(4) // Found(2)
list.search(2) // InsertionPoint(1)