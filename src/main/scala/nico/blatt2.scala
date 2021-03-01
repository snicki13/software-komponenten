package nico

import scala.annotation.tailrec

enum Player {
    case WhitePlayer
    case BlackPlayer
}
import Player._

trait Config[C] {
    extension (c: C) def score(): Int
    extension (c: C) def successors(player: Player): Seq[C]
    extension (c: C) def isFinal(): Boolean
}

class AlphaBeta[C: Config] {
    private def helpMax(configList: Seq[C], player: Player, alpha: Int, beta: Int): (C, Int) = {
        val resultHead = alphaBeta(configList.head, player, alpha, beta)
        if (configList.length > 1) {
            val aNeu = List(resultHead, alpha).max
            if (aNeu < beta) {
                val (resultTailConfig, resultTail) = helpMax(configList.tail, player, aNeu, beta)
                if (resultHead >= resultTail)
                    (configList.head, resultHead)
                else
                    (resultTailConfig, resultTail)
            } else 
                (configList.head, resultHead)
        } else {
            (configList.head, resultHead)
        }
    }
    private def helpMin(configList: Seq[C], player: Player, alpha: Int, beta: Int): (C, Int) = {
        val resultHead = alphaBeta(configList.head, player, alpha, beta)
        if (configList.length > 1) {
            val bNeu = List(resultHead, beta).min
            if (bNeu > alpha) {
                val (resultTailConfig, resultTail) = helpMin(configList.tail, player, alpha, bNeu)
                if (resultHead <= resultTail)
                    (configList.head, resultHead)
                else
                    (resultTailConfig, resultTail)
            } else
                (configList.head, resultHead)
        } else {
            (configList.head, resultHead)
        }
    }
    
    private def alphaBeta(config: C, player: Player, alpha: Int, beta: Int): Int = {
        if (config.isFinal()) {
            config.score()
        } else {
            val (_, result) = player match {
                case WhitePlayer =>
                    helpMax(config.successors(WhitePlayer), BlackPlayer, alpha, beta)
                case BlackPlayer =>
                    helpMin(config.successors(BlackPlayer), WhitePlayer, alpha, beta)
            }
            result
        }
    }

    def bestNextConfig(config: C, player: Player): C = {
        val (nextConfig, _) = player match {
            case WhitePlayer =>
                config.successors(WhitePlayer)
                    .map(c => (c, alphaBeta(c, BlackPlayer, Int.MinValue, Int.MaxValue)))
                    .maxBy {
                        case (_, v) => v
                    }
            case BlackPlayer =>
                config.successors(BlackPlayer)
                    .map(c => (c, alphaBeta(c, WhitePlayer, Int.MinValue, Int.MaxValue)))
                    .minBy {
                        case (_, v) => v
                    }
        }
        nextConfig
    }
}

given VectorConfig: Config[Vector[Int]] with {
    val X: Int = 1
    val O: Int = -1

    extension (configVector: Vector[Int]) def score(): Int = configVector.isWinning() match {
        case Some(WhitePlayer) => 1
        case Some(BlackPlayer) => -1
        case None => 0
    }
    extension (configVector: Vector[Int]) def successors(player: Player): Seq[Vector[Int]] = {
        val tok = if (player == WhitePlayer) X else O
        if (configVector.isFinal()) {
            Nil
        }
        else {
            for (i <- 0 until 9
                 if configVector(i) == 0
                 ) yield (configVector.updated(i, tok))
        }
    }
    extension (configVector: Vector[Int]) def isWinning(): Option[Player] = {
        val threeInARow: Option[(Int, Int, Int)] = Seq(
            (0,1,2), (3,4,5), (6,7,8),
            (0,3,6), (1,4,7), (2,5,8),
            (0,4,8), (2,4,6)
        ).find {
            case (i,j,k) =>
                (configVector(j) == configVector(i)) & (configVector(k) == configVector(i))
        }
        threeInARow match {
            case Some((i, _, _)) if configVector(i) == X =>
                Some(WhitePlayer)
            case Some((i, _, _)) if configVector(i) == O =>
                Some(BlackPlayer)
            case _ =>
                None
        }
    }
    extension (configVector: Vector[Int]) def isFinal(): Boolean = configVector.isFull() || configVector.isWinning().isDefined
    extension (configVector: Vector[Int]) def isFull(): Boolean = !configVector.contains(0)
    extension (configVector: Vector[Int]) def toString: String = configVector.toString
}

val actConfig_1: Vector[Int] = Vector(
    1, -1, 0,
    -1, 1, 0,
    1, -1, 0)
val expected_1 = Vector(
    1, -1, 1,
    -1, 1, 0,
    1, -1, 0)
val actConfig_2: Vector[Int] = Vector(
    1, -1, 0,
    1, 1, -1,
    -1, -1, 0)
val expected_2 = Vector(
    1, -1, 0,
    1, 1, -1,
    -1, -1, 1)
val actConfig_3: Vector[Int] = Vector(
    1, -1, 0,
    1, 1, 0,
    -1, -1, 0)
val expected_3 = Vector(
    1, -1, 0,
    1, 1, 1,
    -1, -1, 0)

val actConfig_4: Vector[Int] = Vector(-1,0,0,-1,1,0, 0,1,0)
val expected_4: Vector[Int] =  Vector(-1,-1,0,-1,1,0, 0,1,0)

val AlphaBetaWithConfig = new AlphaBeta
val newConfig_1 = AlphaBetaWithConfig.bestNextConfig(actConfig_1, WhitePlayer)
val newConfig_2 = AlphaBetaWithConfig.bestNextConfig(actConfig_2, WhitePlayer)
val newConfig_3 = AlphaBetaWithConfig.bestNextConfig(actConfig_3, WhitePlayer)
val newConfig_4 = AlphaBetaWithConfig.bestNextConfig(actConfig_4, BlackPlayer)

val test_printer = (found: Vector[Int], expected: Vector[Int], nr: Int) => {
    val same : Boolean = found.equals(expected)
    println(s"Test $nr: $same")

    if !same then
        println(s"Found   : $found")
        println(s"Expected: $expected")
}

@main def ttt: Unit = {
    test_printer(newConfig_1, expected_1, 1)
    test_printer(newConfig_2, expected_2, 2)
    test_printer(newConfig_3, expected_3, 3)
    test_printer(newConfig_4, expected_4, 4)
}
