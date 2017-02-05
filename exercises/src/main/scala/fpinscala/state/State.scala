package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (if (i < 0) -(i + 1) else i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    ((i.toDouble/(Int.MaxValue + 1)), r)
  }

  def doubleWithMap: Rand[Double] =
    map(nonNegativeInt)(_.toDouble/(Int.MaxValue + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def randIntDouble: Rand[(Int, Double)] =
    map2(int, double)((_,_))

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def randDoubleInt: Rand[(Double, Int)] =
    map2(double, int)((_,_))

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (List.empty, rng)
    else {
      val (i, r1) = rng.nextInt
      val (is, r2) = ints(count-1)(r1)
      (i::is, r2)
    }
  }

  def intsWithSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def intsTailRec(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int)(rng: RNG)(res: List[Int]) : (List[Int], RNG) = {
      if (count == 0) (res, rng)
      else {
        val (i, r1) = rng.nextInt
        go(count-1)(r1)(i::res)
      }
    }
    go(count)(rng)(List())
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }
  }

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs.foldRight[(List[A], RNG)](List(), rng)((h,t) => {
      val (a, r) = h(t._2)
      (a::t._1, r)
    })

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(List[A]()))((h, t) => map2(h, t)(_::_))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}


object State {
  type Rand[A] = State[RNG, A]
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    a <- get
    _ <- set(f(a))
  } yield ()

//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
//    State(
//      (m: Machine) => {
//        val fm = inputs.foldLeft[Machine](m)((accum, r) => r match {
//          case Coin => {
//            if (!accum.locked) accum
//            else if (accum.candies == 0) accum
//            else if (accum.candies > 0) Machine(false, accum.candies, accum.coins + 1)
//            else accum
//          }
//          case Turn => {
//            if (!accum.locked) Machine(true, accum.candies - 1, accum.coins)
//            else accum
//          }
//        })
//        ((fm.candies, fm.coins), fm)
//      }
//    )
//  }
}

import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}
