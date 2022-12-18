
// underscore is positionally matched.
(1 to 5).reduceLeft(_ + _)

// as such, this won't work - (1 to 5).map(_ * _)
// instead use named arguments when you want to use it more than once
(1 to 5).map(x=>x * x)

abstract class Animal {
  def name: String
}
case class Cat(name: String) extends Animal
case class Dog(name: String) extends Animal

def printAnimalNames(animals: List[Animal]): Unit = {
  animals.foreach { animal =>
    println(animal.name)
  }
}

val cats: List[Cat] = List(Cat("Whiskers"), Cat("Tom"))
val dogs: List[Dog] = List(Dog("Fido"), Dog("Rex"))
val catsanddogs: List[Animal] = List(Cat("poocha"), Dog("patty"))

printAnimalNames(cats)
printAnimalNames(dogs)
printAnimalNames(catsanddogs)

class Container[A](value: A) {
  private var _value: A = value
  def getValue: A = _value
  def setValue(value: A): Unit = {
    _value = value
  }
}

//val catContainer: Container[Cat] = new Container(Cat("Felix"))
//val animalContainer: Container[Animal] = catContainer
//animalContainer.setValue(Dog("Spot"))
//val cat: Cat = catContainer.getValue // Oops, we'd end up with a Dog assigned to a Cat

abstract class Printer[-A] {
  def print(value: A): Unit
}
class AnimalPrinter extends Printer[Animal] {
  def print(animal: Animal): Unit =
    println("The animal's name is: " + animal.name)
}

val myCat: Cat = Cat("Boots")

def printMyCat(printer: Printer[Cat]): Unit = {
  printer.print(myCat)
}

val animalPrinter: Printer[Animal] = new AnimalPrinter

printMyCat(animalPrinter)


