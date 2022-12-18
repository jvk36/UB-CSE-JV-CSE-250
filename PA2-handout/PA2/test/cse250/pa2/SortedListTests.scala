/**
 * cse250.pa2.SortedListTests.scala
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

import cse250.adaptors.LectureQueue
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatest.Assertions._

class SortedListTests extends FlatSpec with BeforeAndAfter {
  behavior of "apply"
  it should "get element at index i within the list using zero based indexes" in {
    val myList = new SortedList[Int]
    for (i <- 1 to 5) myList.insert(i)
    assert(myList.length == 5)
    for (i <- 0 to 4) assert(myList(i) == (i+1))
  }
  it should "throw exception for invalid index" in {
    val myList = new SortedList[Int]
    for (i <- 1 to 5) myList.insert(i)
    assertThrows[IllegalArgumentException](myList(5))
    assertThrows[IllegalArgumentException](myList(-1))
  }

  behavior of "insert"
  it should "insert a solo element into list at index 0" in {
    val myList = new SortedList[Int]
    val valToInsert = 5
    myList.insert(valToInsert)
    assert(myList.length == 1)
    assert(myList(0) == valToInsert)
  }
  it should "maintain order when duplicate values are inserted" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(2)
    myList.insert(0)
    assert(myList.length == 3)
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    assert(myList(2) == 2)
  }

  behavior of "processBatch"
  it should "process two insertions" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",0)
    myList.processBatch(jobQueue)
    // Should have inserted the values: 0,0.
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    // Should have removed both copies of 0.
    jobQueue.enqueue("remove",0)
    myList.processBatch(jobQueue)
    assert(myList.length == 0)
  }
  it should "process multiple inserts and removes" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",3)
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",7)
    jobQueue.enqueue("insert",-1)
    jobQueue.enqueue("insert",12)
    myList.processBatch(jobQueue)
    // Should have inserted the values: -1, 0, 0, 3, 7, 12.
    assert(myList.length == 6)
    assert(myList(0) == -1)
    assert(myList(1) == 0)
    assert(myList(2) == 0)
    assert(myList(3) == 3)
    assert(myList(4) == 7)
    assert(myList(5) == 12)
    jobQueue.enqueue("remove",0)
    jobQueue.enqueue("remove", 7)
    myList.processBatch(jobQueue)
    // should have removed both copies of 0 and the copy of 7. So, should have values: -1, 3, 12
    assert(myList.length == 3)
    assert(myList(0) == -1)
    assert(myList(1) == 3)
    assert(myList(2) == 12)
  }

  behavior of "undoLastModification"
  it should "undo the last modification, if any changes has been made" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(2)
    myList.insert(0)
    myList.insert(1)
    myList.remove(0)
    // shaould have 1, 2
    myList.undoLastModification()
    // should now have 0,0,1,2
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    assert(myList(2) == 1)
    assert(myList(3) == 2)
    myList.undoLastModification()
    // should now have 0, 0, 2
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    assert(myList(2) == 2)
  }
  it should "restore state between method calls and ignore state changes happening within a call" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",7)
    jobQueue.enqueue("insert",-1)
    jobQueue.enqueue("insert",12)
    myList.processBatch(jobQueue)
    // should have 7, -1, 12
    myList.undoLastModification()
    // should now have emptyList
    assert(myList.length==0)
  }
}
