package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
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

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)))
      assert(!terrain(Pos(4,11)))
    }
  }

  test("findChar level 1") {
    new Level1 {
      println(startPos)
      assert(startPos == Pos(1,1))
    }
  }

  test("standing Block"){
    new Level1{
      assert(Block(Pos(1,1), Pos(1,1)).isStanding)
    }
  }

  test("legal Block"){
    new Level1{
      assert(!Block(Pos(10,10), Pos(10,10)).isLegal)
    }
  }

  test("start Block"){
    new Level1{
      assert(startBlock.isLegal)
      assert(startBlock.isStanding)
      assert(startBlock       == Block(Pos(1, 1), Pos(1,1)))

      assert(startBlock.right == Block(Pos( 1,  2), Pos(1, 3)))
      assert(startBlock.left  == Block(Pos( 1, -1), Pos(1, 0)))
      assert(startBlock.up    == Block(Pos(-1,  1), Pos(0, 1)))
      assert(startBlock.down  == Block(Pos( 2,  1), Pos(3, 1)))
    }
  }

  test("neighbors"){
    new Level1{
      val blocks = List((startBlock.right, Right),(startBlock.left, Left), (startBlock.up, Up),(startBlock.down, Down))
      assert(startBlock.neighbors == blocks)
    }
  }

  test("legal neighbors"){
    new Level1{
      val blocks = List((startBlock.right, Right),(startBlock.down, Down))
      assert(startBlock.legalNeighbors == blocks)
    }
  }

  test("done"){
    new Level1{
      assert(done(Block(Pos(4,7), Pos(4,7))))
    }
  }

  test("neighborsWithHistory"){
    new Level1 {
      val neighbors = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)) == neighbors)
    }
  }

  test("newNeighborsOnly"){
    new Level1 {
      val newNeighbors = Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream

      val neighbors = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      val explored = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))

      assert(newNeighborsOnly(neighbors,explored) == newNeighbors)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      val sol = solve(solution)
      println("solution")
      println(sol)
      println(goal)
      assert(sol == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      println(solution)
      assert(solution.length == optsolution.length)
    }
  }
}
