package example
import Stream._

trait Stream[+A] {
  /*
    Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the REPL. You can convert to the regular List type in the standard library. You can place this and other functions that operate on a Stream inside the Stream trait.
    def toList: List[A]
  */
  def toList(): List[A] = this match {
    case Empty => List[A]()
    case Cons(h, t) => return (h() :: t().toList())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if(n > 1)=> { return cons(h(), t().take(n-1)) } 
    case Cons(h, _) if(n==1) => { return cons(h(), empty) }
    case _ => empty 
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if(n >= 1) => { 
          return t().drop(n-1)
    }
    case Cons(h, t) if(n == 0) => { 
          return Cons(h, t) 
    }
    case _ => { 
      empty
    }
  }


  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => {
      if(p(h())) {
        return cons(h(), t().takeWhile(p))
      } else {
        return t().takeWhile(p) 
      }
    }
    case _ => empty
  }

  def filter(f: A  => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => {
      if(f(a)) {
        return cons(a, b)
      } else {
        return b
      }
    })

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b)) 

  def append[B>:A](newStream: => Stream[B]): Stream[B] =
    foldRight(newStream)((a, b) => cons(a, b)) 

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((a, b) => f(a).append(b))

  def headOption: Option[A] = ???

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def startsWith[B](s: Stream[B]): Boolean = ???

  def forAll[B](p: A => Boolean): Boolean =
    foldRight(true)((a, b) => { return p(a) && b })

  def takeWhile_1(f:A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => {
    if(f(a)) cons(a, b) else empty
  })

  def foldRight[B](z: =>B)(f: (A,=>B) =>B):B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  /*
  *
     We typically want to cache the values of a Cons node, once they are forced. If we use the Cons data constructor directly, for instance, this code will actually compute expensive(x) twice: 

     val x = Cons(() => expensive(x), tl) val h1 = x.headOption
     val h2 = x.headOption

    We typically avoid this problem by defining smart constructors, which is what we call a function for constructing 
    a data type that ensures some additional invariant or provides a slightly different signature than 
    the “real” constructors used for pattern matching.

    By convention, smart constructors typically lowercase the first letter of the corresponding data constructor.

    Here, our cons smart constructor takes care of memoizing the by-name arguments for the head and tail of the Cons.
    This is a common trick, and it ensures that our thunk will only do its work once, when forced for the first time.
    Subsequent forces will return the cached lazy val.
  */


  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def constant1[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    return tail
  }

	def from(n: Int): Stream[Int] = cons(n, from(n+1))
  
  def fibs(first: Int, second: Int): Stream[Int] = cons(first, fibs(second, first+second))

	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, t)) => cons(h, unfold(t)(f))
    case None => empty
  }

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x+1)) 

  def constantUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x))

  def fibsUnfold(): Stream[Int] =  {
   unfold((0, 1))((s: (Int, Int)) => Some(s._1, (s._2, s._1 + s._2)))
  }

  def mapUnfold[B](f: B => Stream[B]) = {

  }
  /**
   * Exercise 5.13
   *
   * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3),
   * and zipAll.
   * The zipAll function should continue the traversal
   * as long as either stream has more elements
   * --- it uses Option to indicate whether each stream has been exhausted.
   */

  def empty[A]: Stream[A] = Empty

  def apply[A] (as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
