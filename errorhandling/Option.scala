object Option {
  def failingFn(i:Int):Int = {
    val y:Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e:Exception => 43 }
  }
}

object ExecOption {
  def main(args:Array[String]):Unit = {
    println(Option.failingFn(1))
  }
}
