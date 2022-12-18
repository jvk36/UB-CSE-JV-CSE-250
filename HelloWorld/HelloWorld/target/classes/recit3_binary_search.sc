import scala.collection.mutable

def binary_search(seq: collection.mutable.Seq[Int], value: Int): Int = {
  var begin = 0; var end = seq.length;
  while (begin != end) {
    val mid = begin + (end - begin) / 2
    if (value < seq(mid)) { end = mid}
    else if (seq(mid) < value) { begin = mid + 1 }
    else { return mid }
  }
  -1
}

binary_search(mutable.Seq(5, 8, 12, 25, 100), 25)
binary_search(mutable.Seq(5, 8, 12, 25, 100), 7)

def recursive_binary_search(seq: collection.mutable.Seq[Int], value: Int, begin: Int, end: Int): Int = {
  if (begin == end) return -1
  val mid = begin + (end - begin) / 2
  if (value == seq(mid)) return mid
  else if (value < seq(mid)) return recursive_binary_search(seq, value, begin, mid-1)
  else return recursive_binary_search(seq, value, mid+1, end)
}

recursive_binary_search(mutable.Seq(5, 8, 12, 25, 100), 25, 0, 4)
recursive_binary_search(mutable.Seq(5, 8, 12, 25, 100), 7, 0, 4)

