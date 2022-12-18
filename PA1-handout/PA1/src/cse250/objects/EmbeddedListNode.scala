/**
 * cse250.objects.EmbeddedListNode.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * DO NOT MODIFY THIS FILE
 */
package cse250.objects

sealed trait EmbeddedListNode[A] {
  // getter and setter for value.
  def value: Option[A] = this match {
    case n: EmbeddedNode[A] => Some(n._value)
    case _: EmbeddedEmpty[A] => None
  }

  def value_=(newValue: A) = this match {
    case n: EmbeddedNode[A] => n._value = newValue
    case _: EmbeddedEmpty[A] => {}
  }

  // getter and setter for prev
  def prev: Int = this match {
    case n: EmbeddedNode[A] => n._prev
    case _: EmbeddedEmpty[A] => -1
  }

  def prev_=(newPrev: Int) = this match {
    case n: EmbeddedNode[A] => n._prev = newPrev
    case _: EmbeddedEmpty[A] => {}
  }

  // getter and setter for next
  def next: Int = this match {
    case n: EmbeddedNode[A] => n._next
    case _: EmbeddedEmpty[A] => -1
  }

  def next_=(newNext: Int) = this match {
    case n: EmbeddedNode[A] => n._next = newNext
    case _: EmbeddedEmpty[A] => {}
  }
}

class EmbeddedNode[A](var _value: A, var _prev: Int, var _next: Int) extends EmbeddedListNode[A]

class EmbeddedEmpty[A] extends EmbeddedListNode[A]

