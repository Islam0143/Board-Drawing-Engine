package Games

import javax.swing.{JFrame, JPanel}
import scala.annotation.tailrec

def gameChooser(frame: JFrame): Unit = {
  println("Choose a game: ")
  Option(scala.io.StdIn.readInt())
    .filter(Set(1, 2, 3, 4, 5, 6).contains)
    .map{ num =>
      frame.dispose()
      gameEngine(drawers(num), controllers(num), initializers(num), switchFunc(gamesMap(gamesName(num))))
    }
    .getOrElse {
      println("Invalid input")
      System.exit(-1)
    }
}

def gameEngine(drawer: Array[Array[String]] => Array[Array[String]], controller: (String, Array[Array[String]], Int) => Any,
               initializer: () => Array[Array[String]], switchFunc: Int => Int): Unit = {
  @tailrec
  def gameLoop(drawer: Array[Array[String]] => Array[Array[String]], controller: (String, Array[Array[String]], Int) => Any,
               switchFunc: Int => Int, grid: Array[Array[String]], player: Int): Unit = {

    controller(scala.io.StdIn.readLine(), grid, player) match
      case grid: Array[Array[String]] =>
        gameLoop(drawer, controller, switchFunc, drawer(grid), switchFunc(player))
      case function: (Int => Int) =>
        gameLoop(drawer, controller, switchFunc, grid, function.apply(player))
  }
  switchFunc(2)
  gameLoop(drawer, controller, switchFunc, drawer(initializer()), 1)
}