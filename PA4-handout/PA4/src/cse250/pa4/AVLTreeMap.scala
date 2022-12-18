/**
 * cse250.pa5.AVLTreeMap.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: annkonna
 * Person#: 50253772
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa4

import cse250.examples.types.mutable.Map

import collection.mutable.Stack

class AVLTreeMap[K, V]()(implicit ord: Ordering[K]) extends Map[K, V]{
  val _storageTree = new AVLTree[K, V]

  override def addOne(elem: (K, V)): Unit = _storageTree.insert(elem)

  override def removeOne(key: K): Boolean = _storageTree.remove(key)

  override def get(key: K): Option[V] = _storageTree.find(key) match {
    case n: _storageTree.AVLNode[(K, V)] if n != null => Some(n._value._2)
    case null                                         => None
  }

  override def iterator: Iterator[(K, V)] = _storageTree.iterator
}

class AVLTree[K, V]()(implicit ord: Ordering[K]) {

  class AVLNode[A](var _value: A, var _left: AVLNode[A], var _right: AVLNode[A], var _parent: AVLNode[A],
                   var _leftH: Boolean, var _rightH: Boolean)

  var _avlRoot: AVLNode[(K, V)] = null

  def find(elem: K): AVLNode[(K, V)] = {
    var current = _avlRoot
    var found = false
    while (!found && current != null) {
      val currentKey = current._value._1
      if (ord.lt(elem, currentKey)) current = current._left
      else if (ord.lt(currentKey, elem)) current = current._right
      else found = true
    }
    current
  }

  def height(node: AVLNode[(K, V)]): Int = {
    if (node == null || (node._left == null && node._right == null)) return 0
    else {
      val maxLeft = height(node._left)
      val maxRight = height(node._right)
      return 1 + maxLeft.max(maxRight)
    }
  }

  def rotateLeft(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    // Left subtree of (B) becomes right subtree of (A).
    val nodeB = nodeA._right
    nodeA._right = nodeB._left
    if (nodeB._left != null) nodeB._left._parent = nodeA

    // (A) becomes left subtree of (B).
    nodeB._left = nodeA
    val parentA = nodeA._parent
    nodeA._parent = nodeB

    // (B) becomes root.
    nodeB._parent = parentA
    if (nodeB._parent == null) _avlRoot = nodeB
    else {
      if (parentA._left == nodeA) parentA._left = nodeB
      else parentA._right = nodeB
    }

    nodeB
  }

  def rotateRight(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    // Right subtree of (B) becomes left subtree of (A).
    val nodeB = nodeA._left
    nodeA._left = nodeB._right
    if (nodeB._right != null) nodeB._right._parent = nodeA

    // (A) becomes right subtree of (B).
    nodeB._right = nodeA
    val parentA = nodeA._parent
    nodeA._parent = nodeB

    // (B) becomes root.
    nodeB._parent = parentA
    if (parentA == null) _avlRoot = nodeB
    else {
      if (parentA._right == nodeA) parentA._right = nodeB
      else parentA._left = nodeB
    }

    nodeB
  }

  def rotateLeftRight(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val nodeB = nodeA._left
    // Left subtree of (C) becomes right subtree of (B).
    val nodeC = nodeB._right
    nodeB._right = nodeC._left
    if (nodeC._left != null) nodeC._left._parent = nodeB

    // Right subtree of (C) becomes left subtree of (A).
    nodeA._left = nodeC._right
    if (nodeC._right != null) nodeC._right._parent = nodeA

    // (A) becomes right subtree of (C).
    nodeC._right = nodeA
    val parentA = nodeA._parent
    nodeA._parent = nodeC

    // (B) becomes left subtree of (C).
    nodeC._left = nodeB
    nodeB._parent = nodeC

    // (C) becomes root.
    nodeC._parent = parentA
    if (parentA == null) _avlRoot = nodeC
    else {
      if (parentA._left == nodeA) parentA._left = nodeC
      else parentA._right = nodeC
    }

    nodeC
  }

  def rotateRightLeft(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val nodeB = nodeA._right
    // Left subtree of (C) becomes right subtree of (A).
    val nodeC = nodeB._left
    nodeA._right = nodeC._left
    if (nodeC._left != null) nodeC._left._parent = nodeA

    // Right subtree of (C) becomes left subtree of (B).
    nodeB._left = nodeC._right
    if (nodeC._right != null) nodeC._right._parent = nodeB

    // (A) becomes left subtree of (C).
    nodeC._left = nodeA
    val parentA = nodeA._parent
    nodeA._parent = nodeC

    // (B) becomes right subtree of (C).
    nodeC._right = nodeB
    nodeB._parent = nodeC

    // (C) becomes root.
    nodeC._parent = parentA
    if (parentA == null) _avlRoot = nodeC
    else {
      if (parentA._left == nodeA) parentA._left = nodeC
      else parentA._right = nodeC
    }

    nodeC
  }

  def getBF(node: AVLNode[(K, V)]): Int = {
    if (node._rightH == node._leftH) 0
    else if (node._rightH) 1
    else -1
  }

  def incrementBF(node: AVLNode[(K, V)]): Boolean = {
    if (node._leftH) {
      node._leftH = false
      true
    }
    else if (!node._rightH && !node._leftH) {
      node._rightH = true
      true
    }
    else false
  }

  def decrementBF(node: AVLNode[(K, V)]): Boolean = {
    if (node._rightH) {
      node._rightH = false
      true
    }
    else if (!node._rightH && !node._leftH) {
      node._leftH = true
      true
    }
    else false
  }

  def setBF(arrNodes: Array[AVLNode[(K, V)]], arrBFs: Array[Int]): Unit = {
    for (i <- arrNodes.indices) {
      arrNodes(i)._leftH = false
      arrNodes(i)._rightH = false
      if (arrBFs(i) == 1) {
        arrNodes(i)._rightH = true
      }
      if (arrBFs(i) == -1) {
        arrNodes(i)._leftH = true
      }
    }
  }

  def insertBST(elem: (K,V)): AVLNode[(K, V)] = {
    if (_avlRoot == null) {
      _avlRoot = new AVLNode[(K, V)](elem, null, null, null, false, false)
      return _avlRoot
    }

    var current = _avlRoot
    var parNode: AVLNode[(K, V)] = null
    while (current != null) {
      parNode = current
      if (ord.lt(elem._1, current._value._1)) current = current._left
      else if (ord.lt(current._value._1, elem._1)) current = current._right
    }

    current = new AVLNode[(K, V)](elem, null, null, parNode, false, false)
    if (ord.lt(elem._1, parNode._value._1)) parNode._left = current
    else parNode._right = current

    current
  }

  def insert(elem: (K, V)): AVLNode[(K, V)] = {
    val foundNode = find(elem._1)
    if (foundNode != null) {
      foundNode._value = elem
      return foundNode

    }
    val node = insertBST(elem)
    var nodeA = node._parent
    var child = node

    while (nodeA != null) {
      var bfPlusTwo = false
      var bfMinusTwo = false
      if (nodeA._right == child) bfPlusTwo = !incrementBF(nodeA)
      else bfMinusTwo = !decrementBF(nodeA)
      if (getBF(nodeA) == 0) return node

      if (bfPlusTwo && getBF(nodeA._right) == 1) {
        val nodeB = rotateLeft(nodeA)
        setBF(Array(nodeA, nodeB), Array(0, 0))
        return node
      }
      else if (bfPlusTwo && (getBF(nodeA._right) == -1)) {
        val nodeC = rotateRightLeft(nodeA)
        val nodeB = nodeC._right
        if (getBF(nodeC) == 0) setBF(Array(nodeA, nodeB, nodeC), Array(0, 0, 0))
        if (getBF(nodeC) == -1) setBF(Array(nodeA, nodeB, nodeC), Array(0, 1, 0))
        if (getBF(nodeC) == 1) setBF(Array(nodeA, nodeB, nodeC), Array(-1, 0, 0))
        return node
      }
      else if (bfMinusTwo && (getBF(nodeA._left) == -1)) {
        val nodeB = rotateRight(nodeA)
        setBF(Array(nodeA, nodeB), Array(0, 0))
        return node
      }
      else if (bfMinusTwo && (getBF(nodeA._left) == 1)) {
        val nodeC = rotateLeftRight(nodeA)
        val nodeB = nodeC._left
        if (getBF(nodeC) == 0) setBF(Array(nodeA, nodeB, nodeC), Array(0, 0, 0))
        if (getBF(nodeC) == -1) setBF(Array(nodeA, nodeB, nodeC), Array(1, 0, 0))
        if (getBF(nodeC) == 1) setBF(Array(nodeA, nodeB, nodeC), Array(0, -1, 0))
        return node
      }
      else {
        child = nodeA
        nodeA = nodeA._parent
      }
    }
    node
  }

  def getSmallest(node: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    var smallest = node
    while (smallest._left != null) {
      smallest = smallest._left
    }
    smallest
  }

  def remove(key: K): Boolean = {
    val node = find(key)
    if (node == null) return false
    val parent = node._parent
    var rightChild = false
    var currentNode = parent
    if (node._left==null && node._right == null) {
      // remove leaf node
      if (parent == null)
        _avlRoot = null
      else {
        if (parent._left == node) parent._left = null
        else {
          parent._right = null
          rightChild = true
        }
      }
    }
    else if (node._left==null || node._right == null) {
      // remove node with one child
      if (node._left==null) {
        node._value = node._right._value
        node._right = null
        rightChild = true
      }
      else {
        node._value = node._left._value
        node._left = null
      }
      currentNode = node
    }
    else {
      // remove node with two children
      val smallest = getSmallest(node._right)
      node._value = smallest._value
      if (smallest._right == null) {
        if (smallest._parent._left == smallest)
          smallest._parent._left = null
        else {
          smallest._parent._right = null
          rightChild = true
        }

        currentNode = smallest._parent
      } else {
        smallest._value = smallest._right._value
        smallest._right = null
        rightChild = true

        currentNode = smallest
      }
    }

    // fix balance factors from the parent of removed node up to root
    var child: AVLNode[(K,V)] = null
    while (currentNode != null) {
      var bfPlusTwo = false
      var bfMinusTwo = false
      if ( (child == null && rightChild) || (child != null && currentNode._right == child) )
        bfMinusTwo = !decrementBF(currentNode)
      else bfPlusTwo = !incrementBF(currentNode)
      val nodeA = currentNode
      if (!bfPlusTwo && !bfMinusTwo && getBF(currentNode).abs == 1) return true
      if (bfPlusTwo && (getBF(currentNode._right) == 0 || getBF(currentNode._right) == 1)) {
        // perform left rotation and update BF of rotated nodes
        val bRightBFZero = getBF(currentNode._right) == 0
        val nodeB = rotateLeft(nodeA)
        if (bRightBFZero) {
          setBF(Array(nodeA, nodeB), Array(1,-1))
          return true
        }
        else setBF(Array(nodeA, nodeB), Array(0,0))
        child = nodeB
        currentNode = nodeB._parent
      }
      else if (bfPlusTwo && getBF(currentNode._right) == -1) {
        // perform right-left rotation and update BF of rotated nodes
        val nodeC = rotateRightLeft(nodeA)
        val nodeB = nodeC._right
        if (getBF(nodeC) == 0) setBF(Array(nodeA, nodeB, nodeC), Array(0, 0, 0))
        if (getBF(nodeC) == -1) setBF(Array(nodeA, nodeB, nodeC), Array(0, 1, 0))
        if (getBF(nodeC) == 1) setBF(Array(nodeA, nodeB, nodeC), Array(-1, 0, 0))
        child = nodeC
        currentNode = nodeC._parent
      }
      else if (bfMinusTwo && (getBF(currentNode._left) == 0 || getBF(currentNode._left) == -1)) {
        // preform right rotation and update BF or rotated nodes
        val bLeftBFZero = getBF(currentNode._left) == 0
        val nodeB = rotateRight(nodeA)
        if (bLeftBFZero) {
          setBF(Array(nodeA, nodeB), Array(-1,1))
          return true
        }
        else setBF(Array(nodeA, nodeB), Array(0,0))
        child = nodeB
        currentNode = nodeB._parent
      }
      else if (bfMinusTwo && getBF(currentNode._left) == 1) {
        // perform left-right rotation and update BF of rotated nodes
        val nodeC = rotateLeftRight(nodeA)
        val nodeB = nodeC._left
        if (getBF(nodeC) == 0) setBF(Array(nodeA, nodeB, nodeC), Array(0, 0, 0))
        if (getBF(nodeC) == -1) setBF(Array(nodeA, nodeB, nodeC), Array(1, 0, 0))
        if (getBF(nodeC) == 1) setBF(Array(nodeA, nodeB, nodeC), Array(0, -1, 0))
        child = nodeC
        currentNode = nodeC._parent
      }
      else {
        child = currentNode
        currentNode = currentNode._parent
      }
    }

    true
  }
  def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    val _parentStack = {
      val stack = new Stack[AVLNode[(K, V)]]
      var currentNode = _avlRoot
      while (currentNode != null) {
        stack.push(currentNode)
        currentNode = currentNode._left
      }
      stack
    }

    override def hasNext: Boolean = _parentStack.nonEmpty

    override def next(): (K, V) = {
      val originalTop = _parentStack.top
      if (originalTop._right != null) {
        var currentNode = originalTop._right
        while (currentNode != null) {
          _parentStack.push(currentNode)
          currentNode = currentNode._left
        }
      }
      else {
        var recentTop = _parentStack.pop
        while (_parentStack.nonEmpty && recentTop != _parentStack.top._left) {
          recentTop = _parentStack.pop
        }
      }
      originalTop._value
    }
  }
}

