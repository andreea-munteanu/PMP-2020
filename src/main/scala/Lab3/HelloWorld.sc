package Lab3

import com.cra.figaro.language._

object HelloWorld{
  def main(args: Array[String]): Unit ={
    val helloWorldElement = Constant("HelloWorld")
    println(helloWorldElement)
  }
}