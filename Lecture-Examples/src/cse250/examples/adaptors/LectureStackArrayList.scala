package cse250.examples.adaptors

import scala.reflect.ClassTag

class LectureStackArrayList[A: ClassTag]
  extends cse250.examples.types.mutable.StackADT[A] {
  private val _store = new cse250.examples.list.LectureArrayList[A]

  override def push(elem: A): Unit = _store.insert(_store.length, elem)

  override def top: A = _store(_store.length - 1)

  override def pop: A = _store.remove(_store.length - 1)

  override def isEmpty: Boolean = _store.length == 0
}
