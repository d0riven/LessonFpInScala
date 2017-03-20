sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree {
  /**
   * EXERCISE 3.25
   * @問題文
   * 2分木のノード(LeafとBranch)の数を数えるsize関数を記述せよ
   */
  def size[A](tree:Tree[A]):Int = tree match {
    case Leaf(v) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  /**
   * EXERCISE 3.26
   * @問題文
   * Tree[Int]の最大の要素を返すmaximum関数を記述せよ。
   * なおScalaでは、x.max(y)またはx max y を使って2つの整数xとyの最大値を計算できる。
   */
  def maximum(tree:Tree[Int]):Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
   * EXERCISE 3.27
   * @問題文
   * 2分木のルートから任意のLeafまでの最長パスを返すdepth関数を記述せよ。
   */
  def depth[A](tree:Tree[A]):Int = tree match {
    case Leaf(v) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  /**
   * EXERCISE 3.28
   * @問題文
   * 2分木の各要素を特定の関数を使って変更するmap関数を記述せよ。
   * この関数はListの同じ名前のメソッドに類似している。
   */
  def map[A,B](tree:Tree[A])(f:A => B):Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
   * EXERCISE 3.29
   * @問題文
   * size, maximum, depth, mapを一般化し、それらの類似点を抽象化する新しいfold関数を記述せよ。
   * そして、このより汎用的なfold関数を使ってそれらを再実装せよ。
   * このfold関数とListの左畳み込みおよび右畳み込みの間にある類似性を抽出することは可能か。
   */
  def fold[A,B](tree:Tree[A], z:B)(f:(A, B)=> B):B = tree match {
    case Leaf(v) => z
    case Branch(l, r) => f(1
  }
}

object ExcerciseTree {
  def main(args:Array[String]):Unit = {
    // Ex3.25
    println(Tree.size(Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d"))
    )));
    // Ex3.26
    println(Tree.maximum(Branch(
      Branch(Leaf(5), Leaf(25)),
      Branch(Leaf(3), Leaf(27))
    )));
    // Ex3.27
    println(Tree.depth(Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d"))
    )));
    println(Tree.depth(
      Branch(
        Branch(
          Branch(Leaf("a"), Leaf("b")),
          Leaf("c")
        ),
        Branch(
          Leaf("d"),
          Branch(
            Leaf("e"),
            Branch(
              Leaf("f"),
              Branch(
                Leaf("g"),
                Leaf("h")
              )
            )
          )
        )
    )));
    // Ex3.28
    println(Tree.map(
      Branch(
        Branch(Leaf(5), Leaf(25)),
        Branch(Leaf(3), Leaf(27))
      ))
      (x => x * 2)
    );
  }
}
