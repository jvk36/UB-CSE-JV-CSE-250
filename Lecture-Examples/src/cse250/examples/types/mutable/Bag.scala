package cse250.examples.types.mutable

trait Bag[A] {
  /** stores a copy of an element to the bag. */
  def addOne(elem: A): Unit

  /** determines if an element is present */
  def contains(elem: A): Boolean

  /** returns multiplicity of an element */
  def count(elem: A): Int

  /** Removes all copies of the element if present. */
  def subtractAll(elem: A): Unit

}
