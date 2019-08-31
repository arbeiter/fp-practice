package example
import org.scalatest._

class StreamSpec extends FunSuite with DiagrammedAssertions {
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

  test("testFlatMapViaFoldRight with normal values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val f = (x: Int) => Cons(() => x, () => Empty)
    val expected = Stream(1, 2, 3, 4).toList
    val actual = me.flatMap(f).toList
    assert(actual == expected)
  }

  test("testForAll with all false") {
    val me = Stream("x11", "x12", "x3", "x4")
    val p = (x: String) => x.contains("x1")
    val expected = false
    val actual = me.forAll(p)
    assert(actual == expected)
  }

  test("testForAll with all true") {
    val me = Stream(1, 2, 3, 4)
    val p = (x: Int) => x > 0
    val expected = true
    val actual = me.forAll(p)
    assert(actual == expected)
  }

  test("testMapViaFoldRight") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4);
    val f = (x: Int) => x + 1
    val expected = Stream(2, 3, 4, 5).toList
    val actual = me.map(f).toList
    assert(actual == expected)
  }

  test("testMapUnfold") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4);
    val f = (x: Int) => x + 1
    val expected = Stream(2, 3, 4, 5).toList
    val actual = me.mapViaUnfold(f).toList
    assert(actual == expected)
  }

  test("testConstant with take") {
    val me = Stream.constant(1)
    val expected = List(1, 1, 1, 1)
    val actual = me.take(4).toList
    assert(actual == expected)
  }

	test("testFrom with take") {
    val me = Stream.from(4)
    val expected = List(4, 5, 6, 7)
    val actual = me.take(4).toList
    assert(actual == expected)
  }

  test("testFibs with take") {
    val me = Stream.fibs(0, 1)
    val expected = List(0, 1, 1, 2, 3, 5, 8)
    val actual = me.take(7).toList
    assert(actual == expected)
  }

  test("testConstantUnfold with take") {
    val me = Stream.constantUnfold(0)
    val expected = List(0, 0, 0, 0, 0)
    val actual = me.take(5).toList
    assert(actual == expected)
  }

  test("Infinite generator") {
    var ones: Stream[Int] = Stream.empty
    ones = Stream.cons(1, ones)
    val actual = ones.take(5).toList
    val expected =  List(1, 1, 1, 1, 1)
    assert(actual == expected)
  }
}
