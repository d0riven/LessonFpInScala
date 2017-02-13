/**
 * @問題文
 * 指定された比較関数に従ってArray[A]がソートされているかどうかを調べるisSortedを実装せよ
 */
object MyModule {
  def isSorted[A](as:Array[A], ordered:(A, A) => Boolean): Boolean = {
    def loop(n:Int):Boolean =
      if (n >= as.length || as.length == 1) true // 全ての配列を見たら終了、1要素だけなら
      else if (!ordered(as(n), as(n - 1))) false // 叙述関数がfalseを返したら
      else loop(n + 1)

    loop(1)
  }

  def main(args:Array[String]):Unit = {
    // 配列が1個だけ、他の型でテストする必要がありそう
    println(isSorted(Array(1, 2, 3), (cur:Int, pre:Int) => cur > pre)) // 昇順に並んでいる
    println(isSorted(Array(2, 1, 3), (cur:Int, pre:Int) => cur > pre)) // 昇順に並んでいない

    println(isSorted(Array(3, 2, 1), (cur:Int, pre:Int) => cur < pre)) // 降順に並んでいる
    println(isSorted(Array(1, 3, 2), (cur:Int, pre:Int) => cur < pre)) // 降順に並んでいない
  }
}
