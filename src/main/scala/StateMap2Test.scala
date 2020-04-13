import RNG._

object StateMap2Test extends App{

  val rng = Simple(42)
  println(double(rng))
}

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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (myInt, nextRng) = rng.nextInt
    val nnInt = {
      if (myInt == Int.MinValue) Int.MaxValue
      else if (myInt < 0) -myInt
      else myInt
    }
    (nnInt, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(_.toDouble/Int.MaxValue)(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (nextInt, rng1) = rng.nextInt
    val (nextDouble, rng2) = double(rng1)
    ((nextInt, nextDouble), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((nextInt, nextDouble), nextRng) = intDouble(rng)
    ((nextDouble, nextInt), nextRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def inner(i:Int, acc: List[Int])(r: RNG): (List[Int], RNG) = {
      if (i <= 0) (acc, rng)
      else {
        val (nextInt, nextRng) = rng.nextInt
        val newList = nextInt :: acc
        inner(i-1, newList)(nextRng)
      }
    }
    inner(count, List())(rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (x: RNG) => {
      val (v1, rng1) = ra(x)
      val (v2, rng2) = rb(x)
      (f(v1, v2), rng1)
    }

  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
