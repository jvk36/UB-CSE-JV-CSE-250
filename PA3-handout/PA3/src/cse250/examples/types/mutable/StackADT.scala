/**
 * cse250.examples.types.mutable.StackADT.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.examples.types.mutable

trait StackADT[A] {
  def push(elem: A): Unit

  def top: A

  def pop: A

  def isEmpty: Boolean
}
