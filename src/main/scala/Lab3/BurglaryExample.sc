package Lab3

import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._

object Burglary {
  def main(args: Array[String]){
    val burglary = Flip(0.01)
    val earthquake = Flip(0.0001)
    val alarm = CPD(burglary, earthquake,
                    (false, false) -> Flip(0.001),
                    (false, true) -> Flip(0.1),
                    (true, false) -> Flip(0.9),
                    (true, true) -> Flip(0.99))
    val johnCalls = CPD(alarm,
        false -> Flip(0.01),
        true -> Flip(0.7))

    johnCalls.observe(true)
    val alg = Importance(100000, burglary)
    alg.start()
    println("Probability of burglary: " + alg.probability(burglary, true))
    alg.kill
    }
}