package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level0 extends SolutionChecker {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

    val optsolution = List(Down, Right, Up)
  }

  trait Level0bis extends SolutionChecker {
    val level =
      """------
        |--STo-
        |--ooo-
        |--ooo-
        |------""".stripMargin

    val optsolution = List(Down, Right, Up)
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level2 extends SolutionChecker {
    /* terrain for level 2*/

    val level =
      """oooo------
        |oSoo------
        |ooTo------
        |oooo------
        |----------
        |----------""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  test("terrain function level 0") {
    new Level0 {
      assert(!terrain(Pos(0, 0)), "-1,0")
      assert(!terrain(Pos(1, 1)), "1,1")
      assert(terrain(Pos(1, 2)), "1,2")
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }

  test("findChar level 0") {
    new Level0 {
      assert(startPos === Pos(1, 2))
      assert(goal === Pos(1, 3))
    }
  }


  test("findChar level 1") {
    new Level1 {
      assert(startPos === Pos(1, 1))
      assert(goal === Pos(4, 7))
    }
  }

  test("isLegal") {
    new Level1 {
      val b = Block(Pos(1, 2), Pos(1, 2))
      assert(b.isStanding === true)
      assert(b.isLegal === true, s"legal error $b")
      val c = Block(Pos(0, 3), Pos(0, 3))
      assert(c.isStanding === true)
      assert(c.isLegal === false, s"legal error $c")
      val d = Block(Pos(0, 2), Pos(0, 3))
      assert(d.isStanding === false)
      assert(d.isLegal === false, s"legal error $d")
    }
  }

  test("done") {
    new Level1 {
      assert(done(startBlock) === false)
      assert(done(Block(goal, goal.dx(1))) === false)
      assert(done(Block(goal, goal)) === true)
    }
  }


  test("neighbors") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet === Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ))
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet === Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ))
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      assert(newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))) === Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ).toStream)
    }
  }

  test("pathsFromStart - level0") {
    new Level0 {
      pathsFromStart.foreach(println)
      assert((pathsFromStart match {
        case (b, n) #:: xs => b
        case _ => null
      }) === startBlock)
    }
  }

  test("pathsFromStart - level1") {
    new Level1 {
      println(pathsFromStart)
      assert((pathsFromStart match {
        case (b, n) #:: xs => b
        case _ => null
      }) === startBlock)
    }
  }

  test("pathToGoal - level1") {
    new Level1 {
      assert((pathsToGoal match {
        case (b, n) #:: xs => b
        case _ => null
      }) === Block(goal, goal))
    }
  }

  test("solution for level 0") {
    new Level0 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("solution for level 0bis") {
    new Level0bis {
      pathsFromStart.foreach(println)
      println("filter now \n\n")
      pathsFromStart.filter(p => p._1.b1 == goal).foreach(println)
      println("pathsToGoal \n\n")
      println(pathsToGoal)
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution for level 0") {
    new Level0 {
      assert(solution === optsolution)
    }
  }

  test("optimal solution for level 0bis") {
    new Level0bis {
      assert(solution === optsolution)
    }
  }

  test("optimal solution length for level 0") {
    new Level0 {
      assert(solution.length === optsolution.length)
    }
  }


  test("solution exist for level 1") {
    new Level1 {
      assert(pathsToGoal.find(_._2 == optsolution.reverse).size === 1)
    }
  }

  test("solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solution === optsolution)
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }
}
