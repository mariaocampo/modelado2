import org.scalatest.FunSuite

class HelloTest extends FunSuite {

  test("sayHello method works correctly"){
    val hello = new Hello
    assert(hello.sayHello("Maria") == "Hello, Maria!")
  }
}
