/**
 * cse250.pa1.DataEntryStore.scala
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
package cse250.pa1

import cse250.objects.{EmbeddedNode, EmbeddedEmpty, EmbeddedListNode}

class DataEntryStore[A](private val _capacity: Int = 100)
  extends collection.mutable.Seq[A] {
  // These private members should not be modified.
  private val _emptyNode = new EmbeddedEmpty[A]
  private val _dataArray = Array.fill[EmbeddedListNode[A]](_capacity)(_emptyNode)
  private var _headIndex = -1
  private var _tailIndex = -1
  private var _numStored = 0

  // Public getters for private members.
  def dataArray = _dataArray
  def headIndex = _headIndex
  def tailIndex = _tailIndex
  def emptyNode = _emptyNode

  /** Inserts element to tail of list. */
  def insert(elem: A): Unit =
  {
    if (_numStored != _capacity) {
      val index = dataArray.indexOf(emptyNode)
      val node = new EmbeddedNode[A](elem, tailIndex, -1)
      dataArray(index) = node
      if (_numStored != 0) {
        dataArray(tailIndex).next = index
        _tailIndex = index
      }
      else {
        _headIndex = index
        _tailIndex = index
      }
      _numStored = _numStored + 1
    }
    else {
      dataArray(headIndex).value = elem
      val newHeadIndex = dataArray(headIndex).next
      dataArray(headIndex).prev = tailIndex
      dataArray(headIndex).next = -1
      dataArray(tailIndex).next = headIndex
      _tailIndex = headIndex
      _headIndex = newHeadIndex
      dataArray(headIndex).prev = -1
    }
  }

/* tested code adapted from Fall 2019
  {
    if (_numStored==_capacity) {
      val oldHeadIndex = headIndex
      _headIndex = dataArray(_headIndex).next
      dataArray(_headIndex).prev = -1
      dataArray(oldHeadIndex).value = elem
      dataArray(oldHeadIndex).next = -1
      dataArray(oldHeadIndex).prev = tailIndex
      dataArray(tailIndex).next = oldHeadIndex
      _tailIndex = oldHeadIndex

      //      println(s"headIndex=$headIndex, tailIndex=$tailIndex, numStored=$numStored, oldHeadIndex=$oldHeadIndex")
      //      println(s"new element=$elem")
    } else {
      //      var slot = dataArray.find(f=>(f.value == _emptyNode)).get
      var slotIndex = dataArray.indexOf(_emptyNode)
      dataArray(slotIndex) = new EmbeddedNode(elem, -1, -1)
      //      println(s"slotIndex = $slotIndex")
      //      dataArray(slotIndex).value = elem
      if (_numStored == 0) _headIndex = slotIndex
      else {
        dataArray(tailIndex).next = slotIndex
        dataArray(slotIndex).prev = tailIndex
      }
      _tailIndex = slotIndex
      _numStored = _numStored+1
    }
  }
*/


  /** Removes all copies of the given element. */
  def remove(elem: A): Boolean = {
    var flag = false
    var i = headIndex
    while (i != -1) {
      if (dataArray(i).value.get != elem) {
        i = dataArray(i).next
      }
      else {
        if (dataArray(i).next != -1) {
          dataArray(dataArray(i).next).prev = dataArray(i).prev
        }
        else {
          _tailIndex = dataArray(i).prev
        }
        if (dataArray(i).prev != -1) {
          dataArray(dataArray(i).prev).next = dataArray(i).next
        }
        else {
          _headIndex = dataArray(i).next
        }
        val count = i
        i = dataArray(i).next
        dataArray(count) = emptyNode
        flag = true
        _numStored = _numStored - 1
      }
    }
    flag
  }

  /* tested code adapted from Fall 2019
    {
      var i = headIndex
      var bPresent = false
      while (i != -1) {
        if (dataArray(i).value.get == elem) {
          bPresent = true

          if (dataArray(i).next == -1) _tailIndex = dataArray(i).prev
          else dataArray(dataArray(i).next).prev = dataArray(i).prev

          if (dataArray(i).prev == -1) _headIndex = dataArray(i).next
          else dataArray(dataArray(i).prev).next = dataArray(i).next

          var iRem = i
          i = dataArray(i).next
          dataArray(iRem) = _emptyNode
          _numStored = _numStored - 1
        } else  i = dataArray(i).next
      }

      bPresent
    }
  */

  /** Returns the count of nodes containing given entry. */
  def countEntry(entry: A): Int = {
    var nodeNum = 0
    var curPos = headIndex
    while (curPos != -1) {
      if (dataArray(curPos).value.get == entry) {
        nodeNum = nodeNum + 1
      }
      curPos = dataArray(curPos).next
    }
    nodeNum
  }
  /* tested code adapted from Fall 2019
  {
    var count=0
    var i = headIndex
    while (i != -1) {
      if (dataArray(i).value.get == entry) count = count + 1
      i = dataArray(i).next
    }
    count
  }
*/

  /** Gets the element at the specified index. */
  override def apply(idx: Int): A = {
    require(idx >= 0 && idx < _numStored)
    var count = 0
    var curPos = headIndex
    while (count != idx) {
      curPos = dataArray(curPos).next
      count = count + 1
    }
    dataArray(curPos).value.get
  }

  /* tested code adapted from Fall 2019
  {
    require(0 <= idx && idx < _numStored)

    var iLoc = 0
    var i = headIndex
    while (iLoc < idx) {
      iLoc = iLoc + 1
      i = dataArray(i).next
    }

    dataArray(i).value.get
  }
  */

  /** Replaces element at given index with a new value. */
  override def update(idx: Int, elem: A): Unit = {
    require(idx >= 0 && idx < _numStored)
    var curPos = headIndex
    var count = 0
    while (count != idx) {
      curPos = dataArray(curPos).next
      count = count + 1
    }
    dataArray(curPos).value = elem
  }
  /* tested code adapted from Fall 2019.
  {
    require(0 <= idx && idx < _numStored)

    var iLoc = 0
    var i = headIndex
    while (iLoc<idx) {
      iLoc = iLoc+1
      i = dataArray(i).next
    }

    dataArray(i).value = elem
  }
  */

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[A] = new Iterator[A] {
    private var currentIndex = _headIndex

    override def hasNext: Boolean = currentIndex != -1

    override def next(): A = {
      val previousIndex = currentIndex
      currentIndex = _dataArray(currentIndex).next
      _dataArray(previousIndex).value.get
    }
  }

  /** Returns the length of the stored list. */
  override def length: Int = _numStored

  //  override def toString: String = if (_numStored == 0) "" else this.iterator.addString(new StringBuilder, "DataEntryStore: (", ",", ")\n").result()
  override def toString: String = if (_numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}
