package example
import org.scalatest._

class StateSpec extends FunSuite with DiagrammedAssertions {
  test("State should start with S") {
    assert("Hellos".startsWith("H"))
  }

  test("next Int") {
    val seed = 42
    val expected = (16159453, SimpleRNG(1059025964525L))
    val actual = SimpleRNG(42).nextInt
    assert(actual==expected)
  }

  test("non negative int") {
    val rng = SimpleRNG(42)
    val expected = (16159453, SimpleRNG(1059025964525L))
    val actual = RNG.nonNegativeInt(rng)
    assert(actual==expected)
  }

  test("non negative double") {
    val rng = SimpleRNG(42)
    val (expected_num, expected_seed) = (16159453, SimpleRNG(1059025964525L))
    val (num , seed) = RNG.double(rng)
    val max = Int.MaxValue.toDouble
    val prod = max * num 

    assert(expected_num==prod)
    assert(seed==expected_seed)
  }
}
