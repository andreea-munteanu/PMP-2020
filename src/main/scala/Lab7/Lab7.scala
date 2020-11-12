package Lab7

import com.cra.figaro.language.Select
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.language.{Element, Chain, Apply}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance


object golf
{
  // par = {3, 4, 5}
  def main(args: Array[String])
  {
    var par = Array(3.0, 4.0, 5.0, 3.0, 4.0, 4.0, 5.0, 4.0, 3.0, 4.0, 4.0, 3.0, 5.0, 5.0, 5.0, 4.0, 3.0, 4.0)
    val skill = Uniform(0, 8.0/13.0)
    val shots = Array.tabulate(18)((hole: Int) => Chain(skill, (s: Double) =>
      Select(s/8.0 -> (par(hole)-2),
        s/2.0  -> (par(hole)-1),
        s -> par(hole),
        (4.0/5.0) * (1  - (s * 13)/8.0) -> (par(hole)+1),
        (1.0/5.0) * (1  - (s * 13)/8.0) -> (par(hole)+2)
      )))

    val vars = for {i <- 0 until 18}
      yield shots(i)
    val score: Element[Double] = Container(shots:_*).reduce(_ + _)

    // for a)
    def score_above_80(score: Double): Boolean = {
      return (score > 80.0)
    }

    // for b)
    def skill_above_03(skill_val: Double): Boolean = {
      return (skill_val > 0.3)
    }

    // constraint that skill must be above 0.3
    skill.setCondition((level: Double) => level > 0.3)

    // a)
    println("P(score>80):")
    println(Importance.probability(score, score_above_80 _))
    score.observe(80)
    // b)
    println(Importance.probability(skill, skill_above_03 _))

    // skill level is uniformly distributed in [0, 8/13]
    val p1_skill = Uniform(0 , 8.0/13.0)
    val p2_skill = Uniform(0 , 8.0/13.0)

    // shots
    val p1_shots =  Array.tabulate(18)((hole: Int) =>
      Chain(p1_skill, (s: Double) =>
      Select(s/8  -> (par(hole)-2),
        s/2  -> (par(hole)-1),
        s -> par(hole),
        (4.0 /5.0 ) * (1  - (s * 13.0)/8.0 ) -> (par(hole)+1),
        (1.0 /5.0 ) * (1  - (s * 13.0)/8.0 ) -> (par(hole)+2))
      ))

    val p2_shots =  Array.tabulate(18)((hole: Int) =>
      Chain(p2_skill, (s: Double) =>
      Select(s/8  -> (par(hole)-2),
        s/2  -> (par(hole)-1),
        s -> par(hole),
        (4/5) * (1  - (s * 13.0)/8.0) -> (par(hole)+1),
        (1/5) * (1  - (s * 13.0)/8.0) -> (par(hole)+2))
    ))
    val shots_p1 = for {i <- 0 until 18} yield p1_shots(i)
    val shots_p2 = for {i <- 0 until 18} yield p2_shots(i)
    val points_p1 = Array.tabulate(18)((hole: Int) =>
      Apply(shots_p1(hole), shots_p2(hole), (p1: Int, p2: Int) => if (p1 < p2) 1;
                                                                  else if (p1 > p2) -1;
                                                                  else 0))
    val p1 = for {i <- 0 until 18} yield points_p1(i)
    val p2 = for {i <- 0 until 18} yield points_p2(i)
    val total = Container(p1:_*).reduce(_+_)

    def skill_above_0(skill_val: Double): Boolean = {
      return (skill_val > 0.0)
    }

    p1_skill.observe(0.5)
    p2_skill.observe(0.5)
    println(p1_shots)
    println(p2_shots)
  }
}
