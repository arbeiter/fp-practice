package example
import org.scalatest._

class HelloSpec extends FunSuite with DiagrammedAssertions {
  test("Hello should start with H") {
    assert("Hello".startsWith("H"))
  }

  test("testToList with normal values") {
    val me = Stream(1, 2, 3, 4, 5)
    val expected = List(1, 2, 3, 4, 5)
    val actual = me.toList
    assert(actual == expected)
	}

  test("testToList with lazy values") {
    val me = Stream({println("hi1"); 0 + 1},
                    {println("hi2"); 0 + 2},
                    {println("hi3"); 0 + 3},
                    {println("hi4"); 0 + 4})
    val expected = List(1, 2, 3, 4)
    println("after Stream and before toList")
    val actual = me.toList
    assert(actual == expected)
  }

  test("drop test") {
    val me = Stream({ 0 + 1},
                    { 0 + 2},
                    { 0 + 3},
                    { 0 + 4});
    val expected = me.drop(2).toList;
    assert(expected == List(3, 4));
  }

  test("takeWhile test") {
    val me = Stream({1},{2},{3},{4});
    val p = (x: Int) => x % 2 != 0;
    val actual = me.takeWhile(p).toList;
    val expected = List(1,3);
    assert(expected == actual);
  }
  
  test("take test") { val me = Stream({ 0 + 1},
                    { 0 + 2},
                    { 0 + 3},
                    { 0 + 4});
    val expected = me.take(2).toList;
    assert(expected == List(1,2));
  }

	test("takeWhile normal values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val p = (x: Int) => x < 3
    val expected = Stream(1, 2).toList
    val actual = me.takeWhile(p).toList
    assert(actual == expected)
	}
}
