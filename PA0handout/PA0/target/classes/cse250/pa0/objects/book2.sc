def buildMap[A,B](data:Seq[A], f:A=>B): Map[B,A] = {
  var keys = data.map(f)
  var ret:Map[B,A] = Map()
  for (i <- keys.indices) {
    ret = ret + (keys(i) -> data(i))
  }
  ret
}

var data:Seq[Int] = Seq(1,2,3,4,5,6,7,8,9,10)
buildMap(data, (i:Int)=>i*2)

