sealed trait List[+A] // +Aって？ => +が付くと型の特化を許可する = 共変(convariant)
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] // A => Nothing なら head : Nothing, tail: List[Noothing] になってしまうのでは？
// わざわざConsを宣言する旨味って何なんだろう？下のコンパニオンオブジェクトでリスト型を独自の関数で処理しやすくできるから？
// List型だと下の関数って似たようなのを定義し辛いのか？

object List {
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def dropWhileCurry[A](as:List[A])(f:A => Boolean):List[A] = as match {
    case Cons(h, t) if f(h) => dropWhileCurry(t)(f)
    case _ => as
  }

  def foldRight[A,B](as:List[A], z:B)(f:(A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns:List[Double]) = foldRight(ns, 1.0)(_ * _)

  /** Exercise 3.2 */
  def tail[A](as: List[A]): List[A] = as match {
    case Cons(h, t) => t
    case _ => List()
  }

  /** Exercise 3.3 */
  def setHead[A](as:List[A], newHead:A): List[A] =
    Cons(newHead, List.tail(as))

  /** Exercise 3.4 */
  // 解1
  def drop[A](l:List[A], n:Int):List[A] = {
    def go(l:List[A], n:Int):List[A] =
      if(n == 0) l
      else go(List.tail(l), n - 1)
    go(l, n)
  }
  // 解2
  /*
  def drop[A](l:List[A], n:Int):List[A] = l match {
    case Cons(h, t) if n == 0 => l
    case Cons(h, t) if n > 0 => drop(t, n - 1)
    case _ => Nil
  }
  */

  /** Exercise 3.5 */
  // 解1
  def dropWhile[A](l:List[A], f:A => Boolean):List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case Cons(h, t) if !f(h) => l
    case _ => Nil // ここにはマッチしないので、パターンマッチ以外の書き方のほうがよいか？
  }
  // 解2
  /* コンパイルエラー取れないので諦めた
  def dropWhile[A](l:List[A], f:A => Boolean):List[A] = {
    def go(l:List[A]):List[A] = {
      val h:A = l match {
        case Cons(h, t) => h
      }
      val t = List.tail(l)
      if(!f(h)) go(t)
      else t
    }
    println(l)
    go(l)
  }
  */

  // Exercise 3.6
  def init[A](l:List[A]):List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case _ => Nil
  }

  // Exercise 3.9
  def length[A](as:List[A]):Int =
    foldRight(as, 0)((x, y) => 1 + y)

  // Exericise 3.10
  def foldLeft[A,B](as:List[A], z:B)(f:(B, A) => B):B = {
    def go[A](as:List[A], acc:B)(f:(B, A) => B):B = as match {
      case Nil => acc
      case Cons(h, t) => go(t, f(acc, h))(f)
    }
    go(as, z)(f)
  }

  // Exercise 3.11
  def sumWithFoldLeft(as:List[Int]):Int =
    foldLeft(as, 0)(_ + _)
  def productWithFoldLeft(as:List[Double]):Double =
    foldLeft(as, 1.0)(_ * _)
  def lengthWithFoldLeft[A](as:List[A]):Int =
    foldLeft(as, 0)((x, y) => 1 + x)

  // Excercise 3.12
  def reverse[A](as:List[A]):List[A] =
    foldLeft(as, Nil:List[A])((x, y) => Cons(y, x))

  // Exercise 3.13
  // 諦めた
  //def foldRightWithFoldLeft[A,B](as:List[A], z:B)(f:(B, A) => B):B =
  //  foldLeft(as, z)((x:B, y:A) => f(y:A, x:B):B)

  // Excercise 3.14
  def append[A](as:List[A], t:A):List[A] =
    foldRight(as, Cons(t, Nil))(Cons(_,_))

  // Excercise 3.15
  // def flatten[A](as:List[A]*):List[A] = {
  //   def go(acc:List[A], as:List[A]*) {
  //     if(as.isEmpty) acc
  //   }
  //   go(Nil:List[A], as)
  // }
}

/**
 * 3.1
 * @問題文
 * 以下のマッチ式はどのような結果になるか
 *
 * @答え
 * List(1,2,4,5)   => x          => 1
 * List(1,2,3,4,5) => x + y      => 3
 * Nil             => 42
 * List(2,3,4,5)   => h + sum(t) => 14
 * other           => 101
 */
/**
 * 3.2
 * @問題文
 * Listの最初の要素を削除する関数tailを実装せよ。
 * この関数の実行時間が一定であることに注意。
 * ListがNilである場合、実装上の選択肢として他に何があるか。
 * この質問については、次章で再び取り上げる。
 */
/**
 * 3.3
 * @問題文
 * EXERCISE 3.2と同じ考え方にもとづいて、Listの最初の要素を別の値と置き換えるsetHead関数を実装せよ。
 */
/**
 * 3.4
 * @問題文
 * tailを一般化して、リストの先頭からnこの要素を排除するdropという関数に書き換えよ。
 * この関数の実行時間は削除する要素の数にのみ比例することに注意。
 * List全体のコピーを作成する必要はない
 */
/**
 * 3.5
 * @問題文
 * 述語とマッチする場合に限り、Listから(先頭から)その要素までの要素を削除するdropWhileを実装せよ。
 *
 * def dropWhile[A](l:List[A], f:A => Boolean):List[A]
 *
 * @メモ
 * p47見るとちょっと勘違いしていたかもしれない.
 */
/**
 * 3.6
 * @問題文
 * すべてがこのようにうまくいくわけではない。
 * Listの末尾を除くすべての要素で構成されたListを返すinit関数を実装せよ。
 * List(1,2,3,4)が与えられた場合、initはList(1,2,3)を返す。
 * この関数をtailのように一定時間で実装ないのはなぜか。
 *
 * def init[A](l:List[A]):List[A]
 *
 * @一定時間で実装できない理由
 * 単方向リストの特性として末尾に行くにはO(n)が必須なため、
 * 末尾のデータを除くものを取得するには配列の長さ分だけリストの参照を移動する実行する必要があるから
 */
/**
 * 3.7
 * @問題文
 * foldRightを使って実装されたproductは、0.0を検出した場合に、直ちに再帰を中止して0.0を返せるか。
 * その理由を説明せよ。大きなリストでfoldRightを呼び出した場合の短絡の仕組みについて検討せよ。
 * この問題は奥が深いため、第5章で改めて取り上げる。
 *
 * @理由
 * 返せない。なぜならfoldRightはListを最後まで処理する前提で組まれているから。
 * foldRightを変更し、途中で特定の数字が出たら特定の結果を返すという変更を加えれば再帰を中止できるかもしれない。
 */
/**
 * 3.8
 * @問題文
 * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))のように、NilおよびCons自体をfoldRightに渡した場合はどうなるか。
 * これがfoldRightとListのデータコンストラクタとの関係について何を表していると思うか。
 *
 * @考察
 * foldRightでマッチングされるリスト(Cons)とList自体のコンストラクタが同一である。
 * applyもfoldRightを使うことで表現できるということになり、applyをfoldRightで表現したのが上記の関数呼び出しになる。
 */
/**
 * 3.9
 * @問題文
 * foldRightを使ってリストの長さを計算せよ。
 *   def length[A](as:List[A]):Int
 */
/**
 * 3.10
 * @問題文
 * このfoldRightの実装は末尾再帰ではなく、リストが大きい場合はStackOverrideflowErrorになってしまう。
 * これはスタックセーフではないと言う。
 * そうした状況であると仮定し、前章で説明した手法を使って、リスト再帰の総称関数foldLeftを記述せよ。
 * シグネチャは以下のとおり。
 *   def foldLeft[A,B](as:List[A], z:B)(f:(B, A) => B):B
 */
/**
 * 3.11
 * @問題文
 * foldLeftを使ってsum, product, およびリストの長さを計算する関数を記述せよ
 */
/**
 * 3.12
 * @問題文
 * 要素が逆に並んだリストを返す関数を記述せよ。List(1,2,3)が与えられた場合、この関数はList(3,2,1,)を返す。
 * 畳み込みを使って記述できるかどうかを確認すること。
 */
/**
 * 3.13
 * @問題文
 * 難問：foldRightをベースとしてfoldLeftを記述することは可能か。その逆はどうか。
 * foldLeftを使ってfoldRightを実装すると、foldRightを末尾再帰に実装することが可能となり、
 * 大きなリストでもスタックオーバーフローが発生しなくなるので便利である。
 *
 * @答え
 * 実装は可能。
 * 計算の途中の状態で初期状態が変わるものでなければ行けると思う。（そんなパターンがあるかはちゃんと考えてない）
 */
/**
 * 3.14
 * @問題文
 * foldLeftまたはfoldRightをベースにappendを作成せよ
 */
/**
 * 3.15
 * @問題文
 * 難問：複数のリストからなるリストを1つのリストとして連結する関数を記述せよ。
 * この関数の実行時間はすべてのリストの長さの合計に対して線形になるはずである。
 * すでに実装した関数を使ってみること。
 */
object Excercise_3 {
  def main(args: Array[String]) {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(x)
    println(Cons(1,Cons(2,Cons(3, Nil))).tail)
    println(List.tail(List(1,2,3)))
    println(List.setHead(List(1, 2, 3), 4))
    println(List.drop(List(1, 2, 3), 2))
    println(List.dropWhile(List(1, 2, 3), (x:Int) => x == 2))
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x:Int) => x < 4))
    println(List.dropWhileCurry(List(1, 2, 3))(x => x == 2))
    println(List.dropWhileCurry(List(1, 2, 3, 4, 5))(x => x < 4))
    println(List.init(List(1,2,3,4)))
    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    // Ex3.11 sum
    println(List.foldLeft(List(1,2,3), 0)(_ + _))
    println(List.sumWithFoldLeft(List(1,2,3)))
    // Ex3.11 product
    println(List.foldLeft(List(1,2,3,4), 1)(_ * _))
    println(List.productWithFoldLeft(List(1,2,3,4)))
    // Ex3.11 length
    println(List.length(List(1,2,3,4,5,6)))
    println(List.lengthWithFoldLeft(List(1,2,3,4,5,6)))
    // Ex3.12
    println(List.reverse(List(1,2,3,4,5,6)))
    // Ex3.14
    println(List.append(List(1,2), 3))
    // Ex3.15
    println(List.flatten(List(1,2), List(3), List(4, 5, 6)))
  }
}
