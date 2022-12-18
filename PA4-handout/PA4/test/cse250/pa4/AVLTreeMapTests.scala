/**
 * cse250.pa4.AVLTreeMapTests.scala
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

import scala.util.Random

class AVLTreeMapTests extends FlatSpec {
  val testSize = 1000
  val rnd = new Random(25)
  val inputKeys = Array.tabulate(testSize)(i => rnd.nextInt)
  val inputValues = Array.tabulate(testSize)(i => i.toString)
//  val inputKeys = Array.tabulate(testSize)(i => i + 1)
//  val inputValues = Array.tabulate(testSize)(i => i.toString * i)
  behavior of "AVLTreeMap"
  it should "BST Insert (key,value) pairs" in {
    val treeMap = new AVLTreeMap[Int, String]
    var elements = inputKeys.zip(inputValues)

    for ((k, v) <- elements) {
      treeMap._storageTree.insertBST((k, v))
      assert(treeMap.contains(k))
    }
    elements = elements.sortBy(k => k._1)
    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      assert(elem == elements(i))
    }
  }
  it should "rotate Left after BST inserts" in {
    val iKeys = Array(3, 5, 6)
    val iValues = Array(3, 5, 6)
    val treeMap = new AVLTreeMap[Int, Int]
    val elements = iKeys.zip(iValues)
    for ((k, v) <- elements) {
      treeMap._storageTree.insertBST((k, v))
      assert(treeMap.contains(k))
    }
    treeMap._storageTree.rotateLeft(treeMap._storageTree._avlRoot)
    treeMap._storageTree.insertBST((8, 8))
    treeMap._storageTree.insertBST((10, 10))
    treeMap._storageTree.rotateLeft(treeMap._storageTree._avlRoot._right)

    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      //      assert(elem == elements(i))
      println(elem)
    }
  }
  it should "rotate Right after BST inserts" in {
    val iKeys = Array(10, 8, 6)
    val iValues = Array(10, 8, 6)
    val treeMap = new AVLTreeMap[Int, Int]
    val elements = iKeys.zip(iValues)
    for ((k, v) <- elements) {
      treeMap._storageTree.insertBST((k, v))
      assert(treeMap.contains(k))
    }

    treeMap._storageTree.rotateRight(treeMap._storageTree._avlRoot)
    treeMap._storageTree.insertBST((5, 5))
    treeMap._storageTree.insertBST((3, 3))
    treeMap._storageTree.rotateRight(treeMap._storageTree._avlRoot._left)

    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      //      assert(elem == elements(i))
      println(elem)
    }
  }
  it should "rotate Right-Left and output tree in order" in {
    val iKeys = Array(3, 2, 8, 5, 13, 4)
    val iValues = Array(3, 2, 8, 5, 13, 4)
    val treeMap = new AVLTreeMap[Int, Int]
    val elements = iKeys.zip(iValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }
    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      //      assert(elem == elements(i))
      println(elem)
    }
  }
  it should "rotate Left-Right and output tree in order" in {
    val iKeys = Array(13, 8, 18, 3, 10, 12)
    val iValues = Array(13, 8, 18, 3, 10, 12)
    val treeMap = new AVLTreeMap[Int, Int]
    val elements = iKeys.zip(iValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }
    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      //      assert(elem == elements(i))
      println(elem)
    }
  }
  it should "add the (key,value) pairs" in {
    val treeMap = new AVLTreeMap[Int, String]
    var elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }
    for (i <- elements.indices) {
      assert(treeMap.contains(elements(i)._1))
    }
    elements = elements.sortBy(k => k._1)
    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      assert(elem == elements(i))
    }
  }
  it should "should update value correctly" in {
    val iKeys = Array(13, 8, 18, 3, 10, 12)
    val iValues = Array(13, 8, 18, 3, 10, 12)
    val treeMap = new AVLTreeMap[Int, Int]
    val elements = iKeys.zip(iValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }
    treeMap.addOne((10, 24))
    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      //      assert(elem == elements(i))
      println(elem)
    }
  }
  it should "remove the (key,value) pairs" in {
    val treeMap = new AVLTreeMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }
    for (i <- elements.indices) {
      val elem = elements(i)
      println("removing " + elem)
      assert(treeMap.removeOne(elem._1) == true)
      println("removed " + elem)
    }
    assert(treeMap.iterator.hasNext == false)
  }
  it should "iterate to produce all the (key,value) pairs exactly once" in {
    val treeMap = new AVLTreeMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }
    val iter = treeMap.iterator
    while (iter.hasNext) println(iter.next)
  }
  it should "AVL Property - maintain balance after each insert" in {
    val treeMap = new AVLTreeMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for (i <- elements.indices) {
      treeMap.addOne(elements(i))
      // for each node verify the balance is between -1 and 1
      for (j <- 0 to i) {
        val node = treeMap._storageTree.find(elements(j)._1)
        assert(node != null)
        var rHeight = 0
        if (node._right != null) rHeight = 1 + treeMap._storageTree.height(node._right)
        var lHeight = 0
        if (node._left != null) lHeight = 1 + treeMap._storageTree.height(node._left)
        val balance = rHeight - lHeight
        assert(balance >= -1 && balance <= 1)
//        println("Left Height = " + lHeight + ", Right Height = " + rHeight + ", balance = " + balance)
      }
    }
  }
  it should "should maintain balance after Right-Left rotations" in {
    val iKeys = Array(3, 2, 8, 5, 13, 4)
    val iValues = Array(3, 2, 8, 5, 13, 4)
    val treeMap = new AVLTreeMap[Int, Int]
    val elements = iKeys.zip(iValues)
    for (i <- elements.indices) {
      val (k,v) = elements(i)
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
      for (j <- 0 to i) {
        val node = treeMap._storageTree.find(k)
        var rHeight = 0
        var lHeight = 0
        if (node._right != null) rHeight = 1 + treeMap._storageTree.height(node._right)
        if (node._left != null) lHeight = 1 + treeMap._storageTree.height(node._left)
        val balance = rHeight - lHeight
        assert(balance >= -1 && balance <= 1)
      }
    }
  }
  it should "AVL Property - should maintain balance after each remove" in {
    val treeMap = new AVLTreeMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }
    val len = elements.length
    for (i <- elements.indices) {
      val elem = elements(i)
      assert(treeMap.removeOne(elem._1) == true)
      for (j <- i+1 to len-1) {
        val node = treeMap._storageTree.find(elements(j)._1)
        assert(node != null)
        var rHeight = 0
        if (node._right != null) rHeight = 1 + treeMap._storageTree.height(node._right)
        var lHeight = 0
        if (node._left != null) lHeight = 1 + treeMap._storageTree.height(node._left)
        val balance = rHeight - lHeight
        if (balance < -1 || balance > 1) {
          assert(balance >= -1 && balance <= 1)
        }
//        println("Left Height = " + lHeight + ", Right Height = " + rHeight + ", balance = " + balance)
      }
    }
    assert(treeMap.iterator.hasNext == false)
  }
}

