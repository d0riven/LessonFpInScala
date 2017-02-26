/**
 * @問題文
 * curryによる変換を逆向きに行うuncurryを実装せよ。
 * =>は右結合であるため、A => (B => C) は A => B => C と記述できる。
 */
object MyModule {
  def uncurry[A, B, C](f:A => B => C):(A, B) => C =
    (x, y) => f(x)(y)


  def main(args:Array[String]):Unit = {
    val cadd = (x:Int) => ((y:Int) => (x + y):Int)
    println(cadd(1)(2))
    val add = uncurry(cadd)
    println(add(1, 2))
  }
}
