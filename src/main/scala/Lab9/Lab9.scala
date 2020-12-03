package Lab9

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored._

import scala.collection.immutable.Range
import scala.reflect.api.Universe

"""
8.5.3/PPE

Let’s model the progression of a student through a series of 10 chapters of
increasing difficulty. Each chapter has a test. The score of the student on the
test depends on how much she learned from the chapter. In addition, the chap-
ters build on one another, so how much the student learns from one chapter
depends on how much she learned from the previous. Model this situation by
using a hidden Markov model and predict the probability that the student will
pass the last test successfully given that she has passed the first three tests.
"""

object Lab9
{
  val chapters = 10 // 10 chapters --> 10 universes
  // studied: array of 10 bool elements initially set to false (student hasn't studied for any chapter)
  val studied: Array[Element[Boolean]]=Array.fill(chapters)(Constant(false))
  studied(0) = Flip(0.9) // we consider 90% chance to have studied for first chapter
  // marked: array of 10 bool elements initially set to false (student hasn't been graded for any chapter)
  val marked: Array[Element[Boolean]]=Array.fill(chapters)(Constant(false))
  // score: marks gotten by student, initially set to 5
  val score: Array[Element[Int]]=Array.fill(chapters)(Constant(5))
  // passed: array of 10 bool elements initially set to false (student hasn't passed any chapter)
  // we know the student has passed the first 3 tests
  //
  // setting grades (first 3 something must be >5, the rest can be anything)
  score(0) = 10
  score(1) = 10
  score(2) = 8
  val r = new scala.util.Random
  for {i <- 3 until nrCapitole} {
    val lowerThan5 = 1 + r.nextInt(4)
    val greatherThan5 = 5 + r.nextInt(6)
    // if student has studied for chapter i, she gets passing grade (>5), else she fails (gets grade <5)
    score(i) = If(studied(i), greatherThan5, lowerThan5)
  }
  // pased: boolean array to see if student passed chapter i
  val passed: Array[Element[Boolean]]=Array.fill(chapters)(Constant(false))
  passed(0) = true
  passed(1) = true
  passed(2) = true
  // difficulty for each chapter as int, initially set to 1
  val difficulty: Array[Element[Int]]=Array.fill(chapters)(Constant(1))

  // creating new universe
  val initialUniverse = Universe.createNew()

  def transition(studied: Array[Boolean], difficulty: Array[Int]): (Element[(Array[Int], Array[Int])]) = {
    // current dificulty will be >= previous difficulty
    // current learn will be <= previous learn
    for {chapterNo <- 1 until chapters} {
      studied(i) = If(studied(i-1),Flip(0.7),Flip(0.3))
      passed(i) = If(studied(i),Flip(0.8),Flip(0.2))
      difficulty(i) = difficulty(i - 1) * (1 + r.nextDouble(0.8)) // difficulty increases by chapter with a max rate of 1.8
    }
    ^^(studied, difficulty)
  }

  // the joint distribution
  for {step <- 1 until chapters} {
    val newState = Chain(studied(step - 1), difficulty(step - 1), (S: Array[Boolean], D: Array[Int]) => transition(l, i))
  }

  // get to next universe by transition
  def nextUniverse(previous: Universe): Universe = {
    val next = Universe.createNew()
    val previousDifficulty = previous.get[Array[Int]]("difficulty")
    val previousStudied = previous.get[Array[Boolean]]("studied")
    val state = Chain(previousStudied, previousDifficulty, transition _)
    // state will be: (studied, difficulty)
    Apply(state, (s: (Array[Boolean], Array[Int], Array[Int])) => s._1)("studied", next)
    Apply(state, (s: (Array[Boolean], Array[Int], Array[Int])) => s._2)("difficulty", next)
    // return next
    next
  }


  def main(args: Array[String]): Unit = {
    val alg = ParticleFilter(initial, nextUniverse, 2000)
    alg.start()
    for {chapterNo <- 1 to chapters} {
      val evidence = {
        arrivingObservation(time) match {
          case None-> Array()
          case Some(n) -> Array(NameEvidence("score", Observation(n)))
        }
      }
      alg.advanceTime(evidence)
      print("Chapter" + chapterNo + ": ")
      print("Expected learning: " + alg.currentExpectation("score", (i:Int) => i)
    }
    alg.kill()
  }
}



