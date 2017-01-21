package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B =  this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

def filter(f: A => Boolean): Option[A] = this.flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))


  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = sys.error("mock up")

  def parseInsuranceRateQuote( age: String,
                               numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _.map(f)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def map2_1[a,b,c](a: option[a], b: option[b])(f: (a, b) => c): option[c] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x::xs => x.flatMap(xx => sequence(xs).map(_ => xx::_))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x::xs => for {
      h <- x
      t <- sequence(xs)
    } yield h::t
  }

  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => None
    case x :: xs => {
      f(x) match {
        case None => None
        case Some(fx) =>
          traverse(xs)(f) match {
            case None => None
            case Some(gx) => Some(fx :: gx)
          }
      }
    }
  }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => map2(f(x), traverse_1(xs)(f))(_::_)
  }

  def traverse_2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil):Option[List[B]])((a, b) => map2(f(a), b)(_::_))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse_2(a)(x => x)

  def parseInts(a: List[String]): Option[List[Int]] =
    traverse_2(a)(i => Try(i.toInt))
}
