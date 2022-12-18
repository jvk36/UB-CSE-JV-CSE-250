import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class MyArray[A: ClassTag] {
  var dataStore:Array[A] = new Array(10)
}

println("test")
var ma = new MyArray[Int]
for (i <- 0 to 9) ma.dataStore(i) = i*2
ma.dataStore.foreach(println)
println("test1")

def evenElems[T: ClassTag](xs: Vector[T]): Array[T] = {
  val arr = new Array[T]((xs.length + 1) / 2)
  for (i <- 0 until xs.length by 2)
    arr(i / 2) = xs(i)
  arr
}

class MyArrayBuffer[A] {
  var dataStore:ArrayBuffer[A] = new ArrayBuffer(10)
}

println("test")
var mab = new MyArrayBuffer[Int]
for (i <- 0 to 9) mab.dataStore.addOne(i*2)
mab.dataStore.foreach(println)
println("test1")

