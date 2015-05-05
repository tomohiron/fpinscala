package fpinscala.datastructures

object ListSample extends App {

  val ex1: List[Double] = Nil
  val ex2: List[Int] = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))
  println(ex3)

  // EXERCISE 3.1 (p.43)
  val z = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // matched!
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  println(z)

  println(List.tail(List(1, 2, 3, 4, 5)))
  println(List.setHead(List(1, 2, 3, 4, 5), 9))
  println(List.drop(List(1, 2, 3, 4, 5), 1))
  println(List.drop(List(1, 2, 3, 4, 5), 3))
  println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x <= 3))
  println(List.dropWhile(List(1, 2, 3, 4, 5), (_: Int) => true))
}
