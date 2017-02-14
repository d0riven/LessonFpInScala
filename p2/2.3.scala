/**
 * @問題文
 * カリー化(currying)では、引数2つの関数fが、fを部分的に適用する引数1つの関数に変換される。
 * この場合も、コンパイルできる実装は１つだけである。
 * この実装を記述せよ。
 */
object MyModule {
  def curry[A, B, C](f:(A, B) => C):A => (B => C) =
    (x:A) => ((y:B) => f(x, y))


  def main(args:Array[String]):Unit = {
    val add = (x:Int, y:Int) => x + y:Int
    println(add(1, 2))
    val cadd = curry(add)
    println(cadd(1)(2))
  }
}
