
val nums = List(1, 2, 3, 4, 5, 6, 7, 8, 9 , 10)
var i=0
var count = 0
while (i < nums.length) {
  if (nums(i)%2==0) count = count+1
  i = i+1
}
println(count)

count=0
for (i <- nums.indices) {
  if (nums(i)%2==0) count = count+1
}
println(count)

println(nums.count(_%2==0))
println(nums.filter(_%2==0).length)
nums.map(i => {
  if (i%2==0) 1 else 0
}).sum

def evenCount(nums:List[Int]):Int = {
  if (nums.length==0) { 0 }
  else if (nums.length==1) {if (nums(0)%2==0) 1 else 0 }
  else evenCount(nums.take(1)) + evenCount(nums.takeRight(nums.length-1))
}
evenCount(nums)

def add(a:(Int, Int), b: (Int, Int)): Double = {
  var lhs = a._1.toDouble/a._2.toDouble
  var rhs = b._1.toDouble / b._2.toDouble
  lhs+rhs
}
add((1,2),(2,3))

var a1 = Array(1,2,3,4,5,6,7,8,9,10)
var a2 = (1 to 10).toArray
var a3 = Array.tabulate(10)(_+1)
var a4 = Array.from(1 to 10)
var a5 = List(1,2,3,4,5,6,7,8,9,10).toArray


