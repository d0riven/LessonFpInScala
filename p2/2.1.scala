/**
 * @問題文
 * n番目のフィボナッチ数を取得する再帰関数を記述せよ。
 * 最初の2つのフィボナッチ数は0と1である。
 * n番目の数字は常に前の２つの数字の合計となる。
 * この数列は0, 1, 1, 2, 3, 5のように始まる。
 * 再帰関数では、ローカルな末尾再帰関数を使用すること。
 */
object Fibonacci {
  def fib(n:Int):Int = {
    def go(n:Int):Int =
      if (n <= 1) 0
      else if (n == 2) 1
      else go(n - 1) + go(n - 2)

    go(n)
  }

  def main(args:Array[String]):Unit =
    println(calc(3))
}
