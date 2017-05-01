package fpinscala.laziness

import Stream._
sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def toList: List[A] = this.foldRight(List[A]())((a, l) => a :: l)

  def toListTailRec: List[A] = {
    @annotation.tailrec
    def go(in: Stream[A], acc: List[A]): List[A] = {
      in match {
        case Empty => acc
        case Cons(h, t) => go(t(), h() :: acc)
      }
    }

    go(this, List.empty[A]).reverse
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /*
      I was worried about this function because it is not tail recursive.
      However, laziness saves us from any potential stackoverflows.
      The crux is that when we return from take, its second argument is a
      thunk that doesn't get evaluated. Only once we need it do we actually
      run the t().take(n-1). Contrast this to the takeThatIsNotLazy that I
      mistakenly wrote below.
   */
  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n-1))
    }
  // maybe not, but you can do it with unfold!
  def take2(n: Int): Stream[A] =
    Stream.unfold((n, this))({
      case (n, st) =>
        if (n <= 0) None
        else st.headOption.map(a => (a, (n-1, st drop 1)))
    })

  /*
      The reason we don't want this is that when we call take3, it will evaluate go(n, this, Empty)
      which, in the case of 3000, will eval 3000 times, basically evaluating the entire stream. (not what we want).
      Instead, the correct implementation
   */
  def takeThatIsNotLazy(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(m: Int, in: => Stream[A], acc: => Stream[A]): Stream[A] = {
      if (m <= 0) acc
      else in match {
        case Empty => acc
        case Cons(h, t) => go(m-1, t(), acc append Stream(h()))
      }
    }
    go(n, this, Empty)
  }

  // can I do this with a fold?
  def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n-1)
    }

  def takeWhile(p: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A])((a, t) => {
    if (p(a)) Cons(() => a, () => t)
    else t
  })
  def takeWhile2(p: A => Boolean): Stream[A] = Stream.unfold(this)(stream => {
    stream.headOption.flatMap(a => if (p(a))  Option(a, stream drop 1) else None)
  })

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this.foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  // should first argument to f be lazy?
  def map[B](f: A => B): Stream[B] = {
    this.foldRight(Empty: Stream[B])((a, t) => cons(f(a), t))
  }
  def map2[B](f: A => B): Stream[B] = Stream.unfold(this)(xs => {
    xs.headOption.map(f).map(fa => (fa, xs drop 1))
  })

  def filter(p: A => Boolean): Stream[A] = {
    this.foldRight(Empty: Stream[A])((a, t) =>
      if (p(a)) cons(a, t filter p)
      else t filter p
    )
  }

  def append[B >: A](xs: => Stream[B]): Stream[B] = {
    this.foldRight(xs)((a, t) => cons(a, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    this.foldRight(Empty: Stream[B])((a, t) => f(a) append t)
  }

  /*
      Goal is to write a hasSubsequence method that will tell us if a sequence has a given subsequence.
      We could zip the two together, check if the first agree.
      If they do, continue.
      else drop from the first sequence and try again
   */

  def startsWith[B](s: Stream[B]): Boolean = {
    val zipped: Stream[(Option[A], Option[B])] = this zipWith s
    zipped.foldRight(true)({
      case ((None, Some(_)), b) => false
      case ((None, None), b) => b
      case ((Some(_), None), b) => b
      case ((Some(x), Some(y)), b) => x == y && b
    })
  }

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def zipWith[B](str: Stream[B]): Stream[(Option[A], Option[B])] = this.zipWithAll(str)((_,_))

  def zip[B](s2: Stream[B]): Stream[(A, B)] = {
    val streamOfOpt: Stream[Option[(A, B)]] = this.zipWithAll(s2) {
      case (o1, o2) => for {
        x <- o1
        y <- o2
      } yield (x, y)
    }
    streamOfOpt flatMap {
      case None => Stream.empty[(A, B)]
      case Some(x) => Stream(x)
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def add(xs: Stream[Int], ys: Stream[Int]): Stream[Int] = {
    val sum: Option[Int] = for (x <- xs.headOption; y <- ys.headOption) yield x + y
    sum.fold(Empty: Stream[Int])(s => cons(s, add(xs drop 1, ys drop 1)))
  }

  val fibs: Stream[Int] = cons(0, cons(1, add(fibs, fibs drop 1)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(Empty: Stream[A])({
      case (a, s) => cons(a, unfold(s)(f))
    })

  def constant2[A](a: A): Stream[A] = unfold(())(_ => Option(a, ()))
  val ones2 = constant2(1)
  val fibs2: Stream[Int] = unfold((0,1))({
    case (x,y) => Option((x, (y, x+y)))
  })

}