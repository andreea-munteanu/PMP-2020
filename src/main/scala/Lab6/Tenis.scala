package Lab6

import com.cra.figaro.library.atomic.[discrete, continuous]
import com.cra.figaro.language.Chain
import com.cra.figaro.language._
import com.cra.figaro.library.compound.{RichCPD, OneOf, *}
import com.cra.figaro.language.{Flip, Constant, Apply}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import scala.collection.mutable.ListBuffer
import com.cra.figaro.library.compound._

/* (points -->) games --> set --> match
*/

object Tenis {
    /* Creating game model by using the probability that each player wins the point when they serve,
       the probability that each of them wins and the probability that each of them makes a mistake.
       Each probability is a type double.
    */
    class Model(probP1ServeWin: Double, probP1Winner: Double, probP1Error: Double,
                probP2ServeWin: Double, probP2Winner: Double, probP2Error: Double) {

            /* Single tennis rally (or serve) using the truth statement that the first shot is successful and that the
            player serving is player1. The function returns bool
            * */
            def rally(firstShot: Boolean, player1: Boolean): Element[Boolean] = {
                /* If the first shot is the serve (and it's successful) and the player serving is player1 --> probP1ServeWin
                                                                                    (probability that player1 wins the serve)
                   Else if the first shot is the serve (and it's successful) and the player serving is player2 --> probP2ServeWin
                                                                                    (probability that player2 wins the serve)
                   Else if it's not the first serve and it's player1's turn --> probP1Winner (probability that his shot is successful)
                   Else it's not the first serve and it's player2's turn --> probP2Winner (probability that player2's shot is successful)
                * */
                val pWinner = if (firstShot && player1) probP1ServeWin
                              else if (firstShot && !player1) probP2ServeWin
                              else if (player1) probP1Winner
                              else probP2Winner
                /* For the player in turn, we calculate the probability that he misses the shot
                */
                val pError = if (player1) probP1Error
                             else probP2Error
                /* Computing the results */
                val winner = Flip(pWinner)
                val error = Flip(pError)
                /* If winner == true --> only player1 has played;
                   If error == true (which means the serve wasn't successful), it means the 2nd player played too
                   Then we call rally(first shot is not successful, player2)
                * */
                If(winner, Constant(player1),  If(error, Constant(!player1), rally(false, !player1)))
            }
            /* Tennis game.
               Variables: p1Serves = truth statement that the player serving is player1
                          p1Points, p2Points = score of each player
               Returns bool
            * */
            def game(p1Serves: Boolean, p1Points: Element[Int], p2Points: Element[Int]): Element[Boolean] = {
                /* Computes probability that player1 wins point by calling rally() */
                val p1WinsPoint = rally(true, p1Serves)
                /* Computes score for each player: if player wins point, increase score by 1
                * */
                val newP1Points =
                    Apply(p1WinsPoint, p1Points, (wins: Boolean, points: Int) =>
                        if (wins) points + 1 else points)
                val newP2Points =
                    Apply(p1WinsPoint, p2Points, (wins: Boolean, points: Int) =>
                        if (wins) points else points + 1)

                /* Compute winner (winner must have at least 4 points and the difference between the winner and
                the other player must be at least 2
                * */
                val p1WinsGame = Apply(newP1Points, newP2Points, (p1: Int, p2: Int) => p1 >= 4 && p1 - p2 >= 2)
                val p2WinsGame = Apply(newP2Points, newP1Points, (p2: Int, p1: Int) => p2 >= 4 && p2 - p1 >= 2)
                /* game ends when either player wins; the game is then reset
                * */
                val gameOver = p1WinsGame || p2WinsGame
                If(gameOver, p1WinsGame, game(p1Serves, newP1Points, newP2Points))
            }
            /*  Simulates whole tennis game
                Variables:  p1Serves (bool) = player serving is player1
                            p1Sets, p2Sets (int)  = number of won sets for each player
                            p1Games, p2Games (int) = number of won games for each player
                 Returns bool
            * */
            def play( p1Serves: Boolean, p1Sets: Element[Int], p2Sets: Element[Int],
                      p1Games: Element[Int], p2Games: Element[Int]): Element[Boolean] = {
                /* see if player1 wins 1st game*/
                val p1WinsGame = game(p1Serves, Constant(0), Constant(0))
                /* When a player wins a point, reset the number of points for them by changing it to 0 if the player
                has won 5 points up to that moment or increase it by 1 until score = 5
                *  */
                val newP1Games = Apply(p1WinsGame, p1Games, p2Games, (wins: Boolean, p1: Int, p2: Int) =>
                                if (wins) {
                                    if (p1 >= 5) 0
                                    else p1 + 1
                                } else {
                                    if (p2 >= 5) 0
                                    else p1
                                })

                val newP2Games =
                    Apply(p1WinsGame, p1Games, p2Games,
                        (wins: Boolean, p1: Int, p2: Int) =>
                            if (wins) {
                                if (p1 >= 5) 0 else p2
                            } else {
                                if (p2 >= 5) 0 else p2 + 1
                            })
                /*
                   For each player, reset the set counter if there are 5 games played up to that point and if the player wins
                   or keep the sets (int) the same if the opponent wins set
                * */
                val newP1Sets =
                    Apply(p1WinsGame, p1Games, p1Sets,
                        (wins: Boolean, games: Int, sets: Int) =>
                            if (wins && games == 5) sets + 1 else sets)
                val newP2Sets =
                    Apply(p1WinsGame, p2Games, p2Sets,
                        (wins: Boolean, games: Int, sets: Int) =>
                            if (!wins && games == 5) sets + 1 else sets)
                /* Match is finished when either player reaches 2 won sets
                * */
                val matchOver =
                    Apply(newP1Sets, newP2Sets, (p1: Int, p2: Int) => p1 >= 2 || p2 >= 2)
                /*  If match is over, start new game by changing the serving player and resetting the number of won sets
                    and points for each player
                *  */
                If(matchOver,
                    Apply(newP1Sets, (sets: Int) => sets >= 2),
                    play(!p1Serves, newP1Sets, newP2Sets, newP1Games, newP2Games))
            }

            /* Play game considering the first one to serve is player1 and the probabilities at lines 18-19 are all 0
            */
            play(true, Constant(0), Constant(0), Constant(0), Constant(0))

            /* **************************************************************************************************** */

            def ex4(probP1player1: Double, player1: Boolean): Element[bool] = {
            /*  first player to win 6 games wins a set
                first player to win 2 sets wins the match
            *  */
                val player1Sets = 0
                val player2Sets = 0
                val p1Points = 0
                val p2Points = 0
            }


            // argument probP1ServeWin, probP2ServeWin in args of class Model()
            def ex5(player1: Boolean): Element[bool] = {
                /* Game is won then a player reaches 4 points unless both reach 3 (tie)
                *  In that case, the winner is the player moving 2 points ahead */
                val p1WinsPoint = rally(true, p1Serves)
                val newP1Points =
                    Apply(p1WinsPoint, p1Points, (wins: Boolean, points: Int) =>
                        if (wins) points + 1 else points)
                val newP2Points =
                    Apply(p1WinsPoint, p2Points, (wins: Boolean, points: Int) =>
                        if (wins) points else points + 1)
                val p1WinsGame = Apply(newP1Points, newP2Points, (p1: Int, p2: Int) => p1 >= 4 && (p1 != 3 && p2!= 3))
                val p2WinsGame = Apply(newP2Points, newP1Points, (p2: Int, p1: Int) => p2 >= 4 && (p1 != 3 && p2!= 3))
                if (!p1WinsGame && !p2WinsGame)
                    {
                        p1WinsGame = Apply(newP1Points, newP2Points, (p1: Int, p2: Int) => p1 >= 4 && p1 - p2 >= 2)
                        p2WinsGame = Apply(newP2Points, newP1Points, (p2: Int, p1: Int) => p2 >= 4 && p1 - p2 >= 2)
                    }

            }
    }
}
