package Lab11

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored._

import scala.collection.immutable.Range
import scala.reflect.api.Universe

"""
9.6.3./PPP

Assume that 1 in 40 million US citizens become president of the United States.

a) Assume that 50% of presidents are left-handed, compared to 10% of the gen-
eral population. What is the probability someone became the president of
the United States, given that he or she is left-handed?

b) Now assume that 15% of US presidents went to Harvard, compared to 1 in
2,000 for the general population. What is the probability that someone became
the president of the United States, given that he or she went to Harvard?

c) Assuming left-handedness and going to Harvard are conditionally indepen-
dent, given whether someone became president, what’s the probability that
someone became the president of the United States, given that he or she is
left-handed and went to Harvard?
"""

object Lab11
{
//  // uniform distribution
//  def uni(p: Double): Distribution[Boolean] = {
//    uniform.map(_ < p)
//  }
//
//  // bernouli distribution:
//  def bernoulli(p: Double): Distribution[Int] = {
//    uni.map(b => if (b) 1 else 0)
//  }

  val citizenIsPresident = Flip(0.000000025)

  """ Point a): """

  val citizenIsLeftHanded = Flip(0.1)
  val presidentIsLeftHanded = Flip(0.5)
  observe.citizenIsLeftHanded(true)
  val leftHandedCitizenBecomesPresident = VariableElimination.probability(citizenIsLeftHanded, 40000000 * citizenIsLeftHanded)
  println("Probability that someone became president given that they're left-handed: " + leftHandedCitizenBecomesPresident)

  """ Point b): """

  val presidentWentToHarvard = Flip(0.15)
  val citizenWentToHarvard = Flip(0.0005)
  observe.citizenWentToHarvard(true)
  val HarvardCitizenBecomesPresident = VariableElimination.probability(citizenWentToHarvard, 40000000 * citizenWentToHarvard)
  println("Probability that someone became president given that they went to Harvard: " + HarvardCitizenBecomesPresident)

  """ Point c): """
  observe.citizenWentToHarvard(true)
  observe.citizenIsLeftHanded(true)
  observe.presidentWentToHarvard(true)
  val harvard_and_lefthanded_citizen = VariableElimination.probability(leftHandedCitizenBecomesPresident, 40000000 * citizenWentToHarvard * citizenIsLeftHanded)
  println("Probability that someone became president given that they went to Harvard and are left-handed: " + harvard_and_lefthanded_citizen)


}




