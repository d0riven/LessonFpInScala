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

  def lift[A,B](f:A=>B):Option[A] => Option[B] = _ map f
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

  /**
   * Ex4.3
   * 2項関数を使ってOption型の2つの値を統合する総称関数map2を記述せよ。
   * どちらかのOption値がNoneの場合は、戻り値もNoneになる。シグネチャは以下のとおり。
   *
   *   def map2[A,B,C](a:Option[A], b:Option[B])(f:(A,B) => C):Option[C]
   */
  def map2[A,B,C](a:Option[A], b:Option[B])(f:(A,B) => C):Option[C] = (a, b) match {
    case (Some(av), Some(bv)) => Some(f(av, bv))
    case (_, _) => None
  }

  /**
   * Ex4.4
   * Optionのリストを1つのOptionにまとめるsequence関数を記述せよ。
   * 新しいOptionには、元のリストに含まれているすべてのSome値のリストが含まれる。
   * 元のリストにNoneが1つでも含まれていた場合、この関数の結果はNoneになる。
   * それ以外の場合には、すべての値のリストを含んだSomeになる。シグネチャは以下のとおり。
   *
   *    def sequence[A](a:List[Option[A]]):Option[List[A]]
   */
  def sequence[A](a:List[Option[A]]):Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => (hh :: sequence(t))) // どうやって List[Option[A]] => List[A] ?
  }

  /* 多分新しく組むならこんな感じ？
  def hoge(a:List[Option[A]], z:Option[List[A]]) = a match {
    case Cons(Some(a), t) => List.hoge(t, List.append(z, a))
    case _ => None
  }
  */
}

object ExecOption {
  def lookupByName(name:String):Option[Employee] = ???
  def insuranceRateQuote(age:Int, numberOfSpeedingTickets:Int):Double =
    age / numberOfSpeedingTickets // 暫定で実装
  /*
  def parseInsuranceRateQuote(age:String,numberOfSpeedingTickets:String):Option[Double] = {
    var optAge:Option[Int] = Try{age.toInt}
    var optTickets:Option[Int] = Try{numberOfSpeedingTickets.toInt}
    insuranceRateQuote(optAge, optTickets)
  }
  */
  def parseInsuranceRateQuote(age:String,numberOfSpeedingTickets:String):Option[Double] = {
    var optAge:Option[Int] = Try{age.toInt}
    var optTickets:Option[Int] = Try{numberOfSpeedingTickets.toInt}
    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case e:Exception => None}
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
    println(parseInsuranceRateQuote("40", "20"))
    println(parseInsuranceRateQuote("hoge", "20"))
    println(Option.sequence(List(Some(1), Some(2), Some(3))))
  }
}
