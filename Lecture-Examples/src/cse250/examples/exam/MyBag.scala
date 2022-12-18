package cse250.examples.exam

import scala.collection.mutable.ArrayBuffer

class MyBag[A] extends cse250.examples.types.mutable.Bag[A] {

  var _storage = new ArrayBuffer[A]

  /** stores a copy of an element to the bag. */
  override def addOne(elem: A): Unit = _storage.insert(0, elem)

  /** determines if an element is present */
  override def contains(elem: A): Boolean = _storage.contains(elem)

  /** returns multiplicity of an element */
  override def count(elem: A): Int = {
    var ret = 0
    for (i <- _storage.indices) {
      if (_storage(i) == elem) ret = ret + 1
    }
    ret
  }

  /** Removes all copies of the element if present. */
  override def subtractAll(elem: A): Unit = {
    while (true) {
      val r = _storage.indexWhere(e => e == elem)
      if (r != -1) _storage.remove(r)
      else return
    }
  }
}
