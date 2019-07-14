package example
import Stream._

object Hello extends App {
  println("Hello")
}

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

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
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

  def empty[A]: Stream[A] = Empty

  def apply[A] (as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
