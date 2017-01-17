package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Nil list")
      case Cons(_, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error ("Nil list")
      case Cons(_, xs) => Cons (h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, xs) if f(h) => dropWhile(xs, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Can't init Nil")
      case Cons(_, Nil) => Nil
      case Cons(h, xs) => Cons(h, init(xs))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def length3(ns: List[Double]) =
    foldLeft(ns, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldLeft3[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, b:B => b)((a, g) => b => g(f(b, a)))(z)

  def foldRight3[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, b:B => b)((g, a) => b => g(f(a, b)))(z)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append(_, _))

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[A])((x, ac) => Cons(x+1, ac))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[B])((h,t) =>
      if (f(h)) Cons(h, t)
      else t
    )

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, addPairwise(xs, ys))
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  @annotation.tailrec
  def startsWith(l: List[A], s: List[A]): Boolean = (l, s) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if (startsWith(sup, sub)) true
    else sup match {
      case Nil => sub == Nil
      case Cons(x, xs) => hasSubsequence(xs, sub)
    }
  }
}
