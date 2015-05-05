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

  println(List.dropWhile(List(1, 2, 3, 4, 5))(x => x <= 3))
  println(List.dropWhile(List(1, 2, 3, 4, 5))(_ => true))

  println(List.append(List(1, 2, 3), List(4, 5, 6, 7)))
  println(List.init(List(1, 2, 3, 4)))

  val ints = List(1, 2, 3, 4, 5)
  println(List.sum(ints))
  println(List.sum2(ints))
  println(List.sum3(ints))

  val ds = List(1.0, 2.0, 3.0, 4.0, 5.0)
  println(List.product(ds))
  println(List.product2(ds))
  println(List.product3(ds))

  // EXERCISE 3.8 (p.50)
  val ex3_8 = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  println(ex3_8)

  println(List.length(ints))
  println(List.length3(ints))

  println(List.foldLeft(ints, 0)(_ + _))
  println(List.reverse(ints))
}
