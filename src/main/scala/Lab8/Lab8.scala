
package Lab8

import com.cra.figaro.language.Select
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.language.{Element, Chain, Apply}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance


object Lab8 // alarm
{
  def main(args: Array[String]){

    """ a) FIGARO MODEL """

    // probability to forget to set alarm = 10%
    val forgetSetAlarm = Flip(0.1)
    // probability to wake up late after setting up alarm = 10%
    val wakeUpLate = CPD(forgetSetAlarm,
                        (true) -> Flip(0.1),
                        (false) -> Flip (0.9)
                        )
    // probability for bus to be late is 20%
    val busIsLate = Flip(0.2)
    // probability to get to work in time according to the table
    val getToWorkInTime = CPD(wakeUpLate, busIsLate,
                              (true, true) -> Flip(0.1, 0.9),
                              (true, false) -> Flip(0.3, 0.7),
                              (false, true) -> Flip(0.2, 0.8),
                              (false, false) -> Flip(0.9, 0.1),
                              )

    """ b) INTEROGATIONS: """

    """ 1. probability to get to work in time, given that you slept through your alarm """
    wakeUpLate.observe(true)
    println("Probability (1) to get to work in time: " + getToWorkInTime)
    //    OR:
    //    val alg = Importance(30000, wakeUpLate, busIsLate)
    //    alg.start()
    //    println("Probability (2) to get to work in time: " + alg.probability(wakeUpLate, true))
    //    alg.kill

    wakeUpLate.unobserve()

    """ 2. probability to have set up the alarm, given that you got to work in time """

    getToWorkInTime.observe(true)
    println("Probability to have set up your alarm, given that you got to work in time: " + (1-forgetSetAlarm))
    // P(has set alarm) = 1 - P(has forgotten to set up alarm)
    //getToWorkInTime.unobserve()

    """ 3. probability to fall back asleep (to have woken up too late) """

    wakeUpLate.observe(true)
    println(wakeUpLate)
    //wakeUpLate.unobserve()

    """ c) Assuming forgetSetAlarm was incorrectly predicted, how could it be self-improved? """

    // for better predictions of the forgetSetAlarm variable we can use the getToWorkInTime variable, given that
    // in 9/10 cases we get to work in time if the alarm has not been set

    val alg2 = Importance(30000, getToWorkInTime)
    alg2.start()
    println("Correct forgetSetAlarm: " + alg2.probability(getToWorkInTime, true))
    alg2.kill()
  }
}
