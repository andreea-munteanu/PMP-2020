package Lab10

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
8.5.4/PPP

Use a DBN to create a simple economic model of a firm. The DBN has three variables: investment, profit, and capital.
At any time step, capital is equal to the previous capital plus new profit minus new investment. The amount of profit at
any time step is uncertain, but tends to be higher with higher Investment.

Consider different policies where the level of Investment at a given time point is equal to a fixed fraction
of the amount of capital at the previous time point. For a given fixed starting amount of capital, predict the amount
of capital after 10 time steps for different investment policies.

"""

object Lab10
{
  val r = new scala.util.Random

  val time_steps = 11 // universe 0 is sort of an initialization
  val investment: Array[Element[Int]] = Array.fill(length)(Constant(0))
  val capital: Array[Element[Int]] = Array.fill(length)(Constant(0))
  val profit: Array[Element[Int]] = Array.fill(length)(Constant(0))
  // bool variable determining whether profit > investment:
  val profitBTInvestment: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  // bool variable determining firm is profitable:
  val isProfitable: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  // fixed initial capital of 100,000.00
  capital(0) = 100000
  // investment(0) = 0 <-- by line 31
  // in universe 0, profit > investment with 50% chance
  profitBTInvestment(0) = Flip(0.5)
  if profitBTInvestment(0) == true isProfitable(0) = true else isProfitable(0) = false


  for {step <- 1 until time_steps} {
    investment(step) = capital(step - 1) * 0.6 // investment = 60% of previous capital
    // investment(step) = capital(step - 1) * 0.7 // investment = 70% of previous capital
    // investment(step) = capital(step - 1) * 0.8 // investment = 80% of previous capital

    // profit is higher with higher investment
    profit(step) = If(investment(step) > 350000, 50000 + r.nextInt(100000), r.nextInt(100000))

    // we calculate new_capital = previous_capital + profit - investment:
    capital(step) = capital(step - 1) + profit(step) - investment(step)
    profitBTInvestment(step) = If(profit(step) > investment(step), true, false)
    isProfitable(step) = If(profitBTInvestment(step), true, false)
  }


  def main(args: Array[String]) {
    println("Predicted capital\n_______________")
    println("Prior: " + VariableElimination.probability(capital(time_steps - 1), (i: Int) => i > 0))
    println("A posteriori: " + VariableElimination.probability(capital(time_steps - 1), (i: Int) => i > 0))
  }
}




