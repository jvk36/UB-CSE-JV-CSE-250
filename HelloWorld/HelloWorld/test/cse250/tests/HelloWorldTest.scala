package cse250.tests

import org.scalatest.FlatSpec
import cse250.examples.HelloWorld
import cse250.examples.Register

class HelloWorldTest extends FlatSpec {
  "HelloWorld.doThings()" should "return 5" in {
    assert(HelloWorld.doThings() == 5)
  }

  it should "not return 10" in {
    assert(HelloWorld.doThings() != 10)
  }

  "HelloWorld.x" should "have type Float" in {
    assert(HelloWorld.x.isInstanceOf[Float])
  }

  "Register(0).addToValue" should "return the input value" in {
    val reg = Register(0)
    for (i <- 1 to 10000) assert(reg.addToValue(i) == i)
  }
}
