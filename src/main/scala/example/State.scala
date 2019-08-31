package example

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
    val nextRNG = SimpleRNG(newSeed) 
    val n = (newSeed >>> 16).toInt
    (n, nextRNG) 
  }
}


object RNG {

  type Rand[+A] = RNG => (A, RNG)
  def int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

	def randomPair(rng: RNG): (Int,Int) = { 
		val (i1,_) = rng.nextInt
		val (i2,_) = rng.nextInt
		(i1,i2)
	}

  def notNegativeEven(rng: RNG): Rand[Int] = { 
    map(nonNegativeInt)(x => (x-1)%2)
}

  def doubleWithMap(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(x => (x.toDouble) / (Int.MaxValue.toDouble + 1))
}

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    var (i1, nextRNG) = rng.nextInt

    if(i1 == Int.MinValue) {
      i1 = Math.abs(Int.MinValue+1)
    }
    else {
      i1 = Math.abs(i1)
    }
    return (i1, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue.toDouble+1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intNum, tempRNG) = rng.nextInt
    val (doubleNum, nextRNG) = double(tempRNG)
    ((intNum, doubleNum), nextRNG)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (doubleNum, nextRNG) = double(rng)
    val (intNum, tempRNG) = nextRNG.nextInt
    return ((doubleNum, intNum), tempRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2) 
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, accumulator: List[Int], rng: RNG): (List[Int], RNG) = {
      if(n==0) {
        (accumulator, rng)
      } else {
        val (intNum, tempRNG) = rng.nextInt 
        loop(n-1, (intNum::accumulator.reverse).reverse, tempRNG)
      }
    }
    loop(count, List(), rng)
  }
}
