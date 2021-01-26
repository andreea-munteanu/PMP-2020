package Partial2

import scala.collection._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.experimental.normalproposals.Normal
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored._


object Partial2 {
  def main(args: Array[String])
  {
    """ Ex. 1 """

    // base class for markov model
    abstract class State {
      var getUmbrella = Array[Element[Boolean]] = Array.fill(chapters)(Constant(false))
      var weather = Array[Element[String]] = Array.fill(chapters)(Constant('sunny))
    }

    // class for initial state:
    class initialState() extends State {
      // setting initial probabilities for weather to be in a certain way:
      weather(0) = Select(0.5 -> 'sunny, 0.2 -> 'rainy, 0.3 -> 'cloudy)
      getUmbrella(0) = CPD(weather(0),
                            'sunny -> Flip(0.15),
                            'rainy -> Flip(0.75),
                            'cloudy -> Flip(0.65))
    }

    // class for getting next state from current state:
    class NextState(current: State, iteration: Int) extends State {
      // getting value for weather
      weather(iteration) = CPD(weather(chapter - 1),
        'sunny -> Select(0.6 -> 'sunny, 0.1 -> 'rainy, 0.3 -> 'cloudy),
        'rainy -> Select(0.45 -> 'rainy, 0.4 -> 'cloudy, 0.15 -> 'sunny),
        'cloudy -> Select(0.5 -> 'cloudy, 0.35 -> 'rainy, 0.15 -> 'sunny))
      // deciding whether we get an umbrella or not depending on weather:
      getUmbrella(iteration) = CPD(weather(iteration),
        'sunny -> Flip(0.15),
        'rainy -> Flip(0.75),
        'cloudy -> Flip(0.65))
    }

    """ Ex. 2 """
    //  a)

    // computing each new universe depending on the previous one:
    for {universe <- 1 until 6} // 6 for point b), here we should say until 5
      {

      }

    // b)

    weather(4).observe('cloudy)
    weather(5).observe('cloudy)
    println(VariableElimination.probability(getUmbrella(6), true))

    // c)

    weather(5).getUmbrella(false)
    println(VariableElimination.probability(weather(2)), 'rainy)

  }
}

