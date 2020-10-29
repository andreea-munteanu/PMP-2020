package Lab5

import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.language.Chain
import com.cra.figaro.library.compound.{RichCPD, OneOf, *}
import com.cra.figaro.language.{Flip, Constant, Apply}
import com.cra.figaro.algorithm.factored.VariableElimination
import scala.collection.mutable.ListBuffer
import com.cra.figaro.library.compound._

object Cards_5 {
    def main(args: Array[String]) {
        val cards = List(5, 4, 3, 2, 1)
        // each player will play 2 cards instead of 1

        val player1Card1 = discrete.Uniform(cards:_*)
        val player1Card2 =
            Chain(player1Card1, (card: Int) =>
                discrete.Uniform(cards.filter(_ != card):_*))

        val player2Card1 =
            Chain(player1Card1, (card: Int) =>
                discrete.Uniform(cards.filter(_ != card ):_*))
        val player2Card2 =
            Chain(^^(player1Card1,player1Card2,player2Card1), (p: (Int, Int,  Int)) =>
                discrete.Uniform(cards.filter(_ != p._1 ).filter(_ != p._2 ).filter(_ != p._3 ):_*))


        val player1Bet1 =
            RichCPD(player1Card1,
                OneOf(5, 4) -> Flip(0.9),
                OneOf(3) -> Flip(0.5),
                * -> Flip(0.4)
            )

        val player2Bet =
            RichCPD(player2Card1, player1Bet1,
                (OneOf(5, 4), *) -> Flip(0.9),
                (OneOf(3), OneOf(false)) -> Flip(0.6),
                (OneOf(3), OneOf(true)) -> Flip(0.4),
                (*, *) -> Flip(0.1))

        val player1Bet2 =
            Apply(player1Card1, player1Bet1, player2Bet,
                (card: Int, bet11: Boolean, bet2: Boolean) =>
                    !bet11 && bet2 && (card == 5 || card == 4))

        val player1Gain =
            Apply(player1Card1, player2Card1, player1Bet1, player2Bet, player1Bet2,
                (card1: Int, card2: Int, bet11: Boolean,
                 bet2: Boolean, bet12: Boolean) =>
                    if (!bet11 && !bet2) 0.0
                    else if (bet11 && !bet2) 1.0
                    else if (!bet11 && bet2 && !bet12) -1.0
                    else if (card1 > card2) 2.0
                    else -2.0)


    }

} 