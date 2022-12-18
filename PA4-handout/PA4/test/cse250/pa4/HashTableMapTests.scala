/**
 * cse250.pa4.HashTableMapTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 */

package cse250.pa4

import org.scalatest.FlatSpec

class HashTableMapTests extends FlatSpec {
  val testSize = 25000
  val inputKeys = Array.tabulate(testSize)(i => i + 1)
  val inputValues = Array.tabulate(testSize)(i => i.toString * i)
  behavior of "HashTableMap.insert"
  it should "add the (key,value) pairs" in {
    val hashMap = new HashTableMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      hashMap.addOne((k, v))
      assert(hashMap.contains(k))
    }
    val iterator = hashMap.iterator
    val elementSet = collection.mutable.Set[(Int, String)]()
    for (_ <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      elementSet.add(elem)
    }
    for (i <- elements.indices) {
      val elem = elements(i)
      assert(elementSet.contains(elem))
    }
  }
  it should "should update value correctly" in {
    val iKeys = Array(13, 8, 18, 3, 10, 12)
    val iValues = Array(13, 8, 18, 3, 10, 12)
    val hashMap = new HashTableMap[Int, Int]
    val elements = iKeys.zip(iValues)
    for ((k, v) <- elements) {
      hashMap.addOne((k, v))
      assert(hashMap.contains(k))
    }
    hashMap.addOne((10, 24))
    val iterator = hashMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      //      assert(elem == elements(i))
      println(elem)
    }
  }
  it should "remove the (key,value) pairs" in {
    val hashMap = new HashTableMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      hashMap.addOne((k, v))
      assert(hashMap.contains(k))
    }
    val iterator = hashMap.iterator
    val elementSet = collection.mutable.Set[(Int, String)]()
    for (_ <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      elementSet.add(elem)
    }
    for (i <- elements.indices) {
      val elem = elements(i)
      assert(hashMap.removeOne(elem._1) == true)
    }
    assert(hashMap.iterator.hasNext == false)
  }
}

