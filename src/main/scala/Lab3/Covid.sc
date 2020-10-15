package Lab3

import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._

object Covid{
  def main(args: Array[String]){
    val fever = Flip(0.06)
    val cough = Flip(0.04)
    val smell_loss = Flip(0.0001)

    val covid = CPD(fever, cough, smell_loss,
      (false, false, false) -> Flip(0.001),
      (false, false, true) -> Flip(0.06),
      (false, true, false) -> Flip(0.0003),
      (false, true, true) -> Flip(0.05),
      (true, false, false) -> Flip(0.00007),
      (true, false, true) -> Flip(0.04),
      (true, true, false) -> Flip(0.00002),
      (true, true, true) -> Flip(0.2),
      (true, false, true) -> Flip(0.03)
    )

    val verdict = CPD(covid,
      true -> Flip(0.02),
      false -> Flip(0.05)
    )

    // chance to be covid-positive

    verdict.observe(true)

    val alg = Importance(30000, fever, cough, smell_loss)

    alg.start()
    println("Prob. fever: " + alg.probability(fever,true))
    println("Prob. cough: " + alg.probability(cough,true))
    println("Prob. smell loss: " + alg.probability(smell_loss,true))
    println("Prob. of covid: " + alg.probability(verdict,true))
    alg.kill

    // no cough, no fever, no smell_loss --> change to be asymptomatic

    fever.observe(false)
    cough.observe(false)
    smell_loss.observe(false)
    verdict.observe(true)

    val confirmed = Flip(0.05)
    val alg2 = Importance(30000, confirmed)

    println("Chance to be covid-positive and asymptomatic: " + alg2.probability(confirmed,true))

    alg2.kill

  }
}