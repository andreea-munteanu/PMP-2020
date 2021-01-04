package Lab12

import scala.collection._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.experimental.normalproposals.Normal
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored._
import scala.chart.api._

"""
11.6/PPP, ex. 1,2,3,4,5
"""

object Lab12 extends App with scala.chart.module.Charting {

  val x0 = Apply(Normal(0.75, 0.3), (d: Double) => d.max(0).min(1))
  val y0 = Apply(Normal(0.4, 0.2), (d: Double) => d.max(0).min(1))
  val x = Flip(x0)
  val y = Flip(y0)
  val z = If(x === y, Flip(0.8), Flip(0.2))


  """ ################################################ ex. 1 ###################################################### """
  
  z.observe(false)

  """ a) run variable elimination to compute the exact posterior probability that y is true, given the observation of z"""

  println("Probability: " + VariableElimination.probability(y, true))
  
  """ b) run importance sampling using 1000, 2000, ..., 10,000 examples. for each # of samples, run the experiment #100.
     Compute the root-mean-square error of importance of sampling with the given number of samples """

  val samples = for {example<- 1000 until 10000 by 1000} 
  yield {
    var squaredError: Float = 0.0
    for {experiment <- 1 until 100 by 1} 
    {
      val imp = Importance(example, y)
      imp.start()
      val imp_2 = imp.probability(y, true)
      val posterior_prob = VariableElimination.probability(y, true)
      val diff = posterior_prob - imp_2
      squaredError += diff * diff
    }
    val root_mean_square_error = math.sqrt(squaredError/100.0)
    println(example + " samples: root_mean_square_error = " + root_mean_square_error)
    (example, root_mean_square_error)
  }

  """ c) plot the root-mean-square error on a chart """

  val chart = XYLineChart(samples)
  chart.show()
  " The more samples we have, the lower the rate of error decrease"

  """ ################################################ ex. 2 ###################################################### """


  val samples_2 = for {i <- 10000 until 100000 by 10000}
  yield {
    var squaredError: Float = 0.0
    for {experiment <- 1 until 100 by 1} {
      Universe.createNew()
      val x = Flip(0.8)
      val y = Flip(0.6)
      val z = If(x === y, Flip(0.9), Flip(0.1))
      z.observe(false)
      val mh = MetropolisHastings(i, ProposalScheme.default, y)
      mh.start()
      val mhAnswer = mh.probability(y, true)
      val diff = veAnswer - mhAnswer
      squaredError = squaredError + diff * diff
    }
    val root_mean_square_error = math.sqrt(squaredError / 100)
    println(i + " samples: root_mean_square_error = " + root_mean_square_error)
    (i, root_mean_square_error)
  }

  val chart2 = XYLineChart(samples_2)
  chart2.show()


  """ ################################################ ex. 3  ###################################################### """

  x1 = Flip(0.999)
  y1 = Flip(0.99)
  z1 = If(x1 === y1, Flip(0.9999), Flip(0.001))

  z1.observe(false)

  """ a) Run variable elimination to get the exact posterior probability of y. """

  println(VariableElimination.probability(y1, true))

  """ b) Run importance sampling with 1,000,000 samples. """

  val squaredError1: Float = 0.0
  val imp = Importance(1000000, y1)
  imp.start()
  val impAnswer = imp.probability(y1, true)
  val diff = veAnswer - impAnswer
  squaredError1 = squaredError + diff * diff

  val root_mean_square_error1 = math.sqrt(squaredError/100.0)
  println(1000000 + " samples: root_mean_square_error = " + root_mean_square_error1)


  """ ################################################ ex. 4  ###################################################### """

  squaredError1: Float = 0.0
  Universe.createNew()
  val x = Flip(0.999)
  val y = Flip(0.99)
  val z = If(x === y, Flip(0.9999), Flip(0.0001))
  z.observe(false)
  val mh = MetropolisHastings(10000000, ProposalScheme.default, y)
  mh.start()
  val mhAnswer = mh.probability(y, true)
  val diff = veAnswer1 - mhAnswer
  squaredError1 = squaredError1 + diff * diff
  root_mean_square_error1 = math.sqrt(squaredError / 100)
  println(1000000 + " samples: root_mean_square_error = " + root_mean_square_error1)

  """ ################################################ ex. 5  ###################################################### """

  val scheme = DisjointScheme(
    0.1 -> (() => ProposalScheme(z1)),
    0.1 -> (() => ProposalScheme(z2)),
    0.8 -> (() => ProposalScheme(x, y))
  )


}


