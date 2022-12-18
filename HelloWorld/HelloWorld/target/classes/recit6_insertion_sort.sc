def insertion_sort_array(seq: Array[Int]): Array[Int] = {
  val sortedSeq = new collection.mutable.ArrayBuffer[Int]
  // Insert element in sorted order.
  for (elem <- seq) {
    var index = sortedSeq.indexWhere(e => e >= elem)
    index = if(index >= 0) index else sortedSeq.length
    sortedSeq.insert(index, elem)
  }
  sortedSeq.toArray
}

insertion_sort_array(Array(5, 9, 1, 100, 7))

def insertion_sort_list(seq: List[Int]): List[Int] = {
  val sortedSeq = new collection.mutable.ListBuffer[Int]
  // Insert element in sorted order.
  for (elem <- seq) {
    var index = sortedSeq.indexWhere(e => e >= elem)
    index = if(index >= 0) index else sortedSeq.length
    sortedSeq.insert(index, elem)
  }
  sortedSeq.toList
}

insertion_sort_list(List(5, 9, 1, 100, 7))

def insertion_sort_array_improved(seq: Array[Int]): Array[Int] = {
  val sortedSeq = new collection.mutable.ArrayBuffer[Int]
  // Insert element in sorted order.
  for (elem <- seq) {
    var index = sortedSeq.search(elem).insertionPoint
    index = if(index >= 0) index else sortedSeq.length
    sortedSeq.insert(index, elem)
  }
  sortedSeq.toArray
}

insertion_sort_array_improved(Array(5, 9, 1, 100, 7))

