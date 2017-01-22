package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList(): List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toListTailRec(): List[A] =  {
    @annotation.tailrec
    def go(s: Stream[A], res: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h()::res)
      case _ => res
    }
    go(this, List()).reverse
  }

  def toListBuf(): List[A] = {
    @annotation.tailrec
    val buf = new collection.mutable.ListBuffer[A]
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf.append(h())
        go(t())
      case _ => buf.toList
    }
    go(this)
  }


  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty[A])
    case _ => empty[A]
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((a,b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a,b) => if (p(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)(cons(_, _))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B]) =
    unfold((this, s2)) {
      case (Cons(h1, t1), Empty) => Some(((Some(h1())), None), (t1(), empty))
      case (Empty, Cons(h2, t2)) => Some((None, (Some(h2()))), (empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty).forAll{
      case (Some(a), Some(b)) if a == b => true
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case p => Some((p, p.drop(1)))
    } append(Stream(empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight[(B, Stream[B])]((z, Stream(z)))((a, b) => {
      lazy val b1 = b
      val r = f(a, b1._1)
      (r, cons(r, b1._2))
    })._2
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

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons[A](() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fibs = {
    def go(p1: Int, p2: Int): Stream[Int] =
      cons(p1, go(p2, p1 + p2))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty
    }

  def unfold_1[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold[Stream[A]](empty)((p: (A, S)) => cons(p._1, unfold_1(p._2)(f)))

  def unfold_2[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A, S)) => cons(p._1, unfold_2(p._2)(f))).getOrElse(empty)

  val fibs_1 =
    unfold((0, 1))((s: (Int, Int)) => Some(s._1, (s._2, s._1 + s._2)))

  val fibs_2 =
    unfold((0, 1)){ case (f1, f2) => Some(f1, (f2, f1 + f2))}

  def from_1(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s+1))

  def constant_1[A](a: A): Stream[A] =
    unfold(a)(s => Some(s, s))

  val ones_1: Stream[Int] = unfold(1)(_ => Some(1,1))
}