package fpinscala.datastructures

sealed trait List[+A] // +Aって？ => +が付くと型の特化を許可する = 共変(convariant)
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] // A => Nothing なら head : Nothing, tail: List[Noothing] になってしまうのでは？
// わざわざConsを宣言する旨味って何なんだろう？下のコンパニオンオブジェクトでリスト型を独自の関数で処理しやすくできるから？
// List型だと下の関数って似たようなのを定義し辛いのか？

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
