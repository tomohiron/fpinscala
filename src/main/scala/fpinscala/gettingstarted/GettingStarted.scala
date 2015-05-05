package fpinscala.gettingstarted

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  def factorial0(n: Int): Int =
    if (n == 0) 1
    else n * factorial0(n - 1)

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  // EXERCISE 2.1 (p.26)
  def fib0(n: Int): Int =
    if (n == 0) 0
    else if (n == 1) 1
    else fib0(n - 1) + fib0(n - 2)

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirstString(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  // EXERCISE 2.2 (p.30)
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n + 1 >= as.length) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  // EXERCISE 2.3 (p.34)
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  // EXERCISE 2.4 (p.34)
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // EXERCISE 2.5 (p.34)
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))

    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))

    //    println(fib(5))
    //    println((0 to 10).map(fib))

    val ss = Array("Jan", "Feb", "Mar", "Apr", "May", "Jun")
    println(findFirstString(ss, "May"))
    println(findFirst(ss, (s: String) => s == "May"))
    println(findFirst(Array(7, 9, 3), (x: Int) => x == 9))

    def ordered(x: Int, y: Int): Boolean = x <= y
    println(isSorted(Array(7, 9, 3), ordered))
    println(isSorted(Array(3, 7, 9), ordered))

    println(partial1(1, (a: Int, b: Int) => a + b)(2))

    val f = (x: Double) => math.Pi / 2 - x
    //    val cos = f andThen math.sin
    val cos = compose(math.sin, f)
    println(cos(0))

    println(curry((a: Int, b: Int) => a + b)(1)(2))
    println(uncurry((a: Int) => (b: Int) => a + b)(1, 2))
  }
}