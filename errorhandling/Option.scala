//sealed trait Option[+A]
trait Option[+A] {
  def map[B](f:A => B):Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f:A => Option[B]):Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f:A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]

case class Employee(name:String, department:String)

object Option {
  def failingFn(i:Int):Int = {
    val y:Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e:Exception => 43 }
  }

  def mean(xs:Seq[Double]):Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs:Seq[Double]):Option[Double] = {
    //val m = mean(xs)
    mean(xs) flatMap (m => mean(xs.map(s => math.pow(m - s, 2))))
  }
}

object ExecOption {
  def lookupByName(name:String):Option[Employee] = ???
  /**
   * Ex4.2
   * @問題文
   * flatMapをベースとしてvariance関数を実装せよ。シーケンスの平均をm、
   * シーケンスの各要素をxとすれば、分散はmath.pow(x - m, 2)の平均となる。
   */
  def main(args:Array[String]):Unit = {
    //println(Option.failingFn(1))
    //val joeDepartment:Option[String] = lookupByName("Joe").map.(_.department)
    println(Option.variance(Array(1, 2, 3, 4, 5)))
  }
}
