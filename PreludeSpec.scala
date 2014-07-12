import scala.language.higherKinds

import scala.util.{Failure, Success, Try}

object PreludeSpec {

  type Test = () => Try[Unit]

  def simple() = Success(())


  val tests: Seq[(String, Test)] = Seq(
    ("simple", simple _)
  )

  def main(args: Array[String]): Unit = {
    println("Testing...")
    tests foreach { case (name, test) =>
      print("  " + name + " => ")
      val res = test()
      println(res)
    }
  }
}
