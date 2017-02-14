/**
 * @問題文
 * 2つの関数を合成する高階関数を実装せよ。
 */
object MyModule {
  def compose[A, B, C](f:B => C, g:A => B):A => C =
    (x:A) => f(g(x))

  def main(args:Array[String]):Unit = {
    val square = (x:Int) => x * x:Int
    val cube = (x:Int) => x * x * x:Int
    println(compose(square, cube)(2)) // (2 ^ 3) * (2 ^3) = 8 ^ 2 = 64
  }
}

