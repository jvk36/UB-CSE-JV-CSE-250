/**
 * cse250.pa2.SortedList.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import cse250.adaptors.LectureStack

class SortedList[A] (implicit _comp: Ordering[A]) extends collection.Seq[A] {
  // Updates the toString to mention our class name instead of Seq.
  override protected[this] def className = "SortedList"

  // Use _storageList to maintain the sorted list.
  var _storageList: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  // ---------- MAKE CHANGES BELOW ----------

  /** Gets element at position i within the list. */
  override def apply(i: Int): A = {
    require(i >= 0 && i < _storageList.length)
    _storageList(i)
  }

  /** Gets the number of elements stored within the list. */
  override def length: Int = _storageList.length

  /** Returns an Iterator that can be used only once. */
  override def iterator: Iterator[A] = _storageList.iterator

  var undoStates: LectureStack[cse250.list.ImmutableLinkedList[A]] = new LectureStack[cse250.list.ImmutableLinkedList[A]]

  /**
   * Inserts one copy of elem into the list in non-decreasing order.
   * @param elem element to be inserted.
   */
  def insert(elem: A): Unit = {
    var idx = _storageList.indexWhere(p => _comp.lt(elem, p))
    if (idx == -1) idx = _storageList.length
    undoStates.push(_storageList)
    _storageList = _storageList.inserted(idx, elem)
  }

  /**
   * Removes all copies of elem from the list.
   * @param elem element to be removed.
   * @return true if any change has been made, and false otherwise.
   */
  def remove(elem: A): Boolean = {
    var idx = _storageList.indexOf(elem)
    var ret = false
    val state = _storageList
    while (idx != -1) {
      ret = true
      _storageList = _storageList.removed(idx)
      idx = _storageList.indexOf(elem)
    }

    if (ret) undoStates.push(state)

    ret
  }

  /** Takes in a queue of valid operations to perform. Each pair has the form:
   *      (OP,elem)
   *  where:
   *      OP will be the string "insert" or "remove"
   *      elem will be a value of type A to use as the argument to OP. */
  def processBatch(operations: cse250.types.mutable.QueueADT[(String,A)]): Unit = {
    var stateChange = false
    var state = _storageList
    while (!operations.isEmpty) {
      val op = operations.dequeue
      if (op._1 == "insert") {
        insert(op._2)
        undoStates.pop
        stateChange = true
      }
      else {
        var ret = remove(op._2)
        if (ret) {
          undoStates.pop
          stateChange = true
        }
      }
    }

    if (stateChange) undoStates.push(state)

  }

  /** Undo the last modification, if any change has been made.
   * If no change to undo exists, throw an IllegalArgumentException.
   */
  def undoLastModification(): Unit = {
    require(!undoStates.isEmpty)
    _storageList = undoStates.pop
  }
}
