package Games

import javax.swing.{JFrame, JPanel}
import scala.annotation.tailrec
import scala.collection.immutable.HashMap


def gameChooser(frame: JFrame, panel: JPanel): Unit = {
  println("Choose a game: ")
  Option(scala.io.StdIn.readInt())
    .filter(Set(1, 2, 3, 4, 5, 6).contains)
    .map(num => gameEngine(drawers(num), controllers(num), initializers(num), switchFunc(gamesMap(gamesName(num))), frame, panel))
    .getOrElse {
      println("Invalid input")
      System.exit(-1)
    }
}

def gameEngine(drawer: (Array[Array[String]], JFrame, JPanel) => Array[Array[String]], controller: (String, Array[Array[String]], Int) => Any,
               initializer: () => Array[Array[String]], switchFunc: Int => Int, frame: JFrame, panel: JPanel): Unit = {
  @tailrec
  def gameLoop(drawer: (Array[Array[String]], JFrame, JPanel) => Array[Array[String]], controller: (String, Array[Array[String]], Int) => Any,
               initializer: () => Array[Array[String]], switchFunc: Int => Int, frame: JFrame, panel: JPanel, grid: Array[Array[String]], player: Int): Unit = {

    controller(scala.io.StdIn.readLine(), grid, player) match {
      case grid: Array[Array[String]] =>
        gameLoop(drawer, controller, initializer, switchFunc, frame, panel, drawer(grid, frame, panel), switchFunc(player))
      case n: Int =>
        gameLoop(drawer, controller, initializer, switchFunc, frame, panel, grid, switchFunc(n))
    }
  }
  switchFunc(2)
  gameLoop(drawer, controller, initializer, switchFunc, frame, panel, drawer(initializer(), frame, panel), 1)
}

def gamesMap(name: String): Int = name match { //maps the game name to its number of players
  case "Tic Tac Toe" | "Chess" | "Checkers" | "Connect 4" => 2
  case "Sudoku" | "8 Queens" => 1
}

def switchFunc(player: Int): Int => Int = HashMap(
  1 -> switch1Player _,
  2 -> switch2Players _
)(player)

def gamesName(number: Int): String = HashMap( //maps the game input number to its name
  1 -> "Tic Tac Toe",
  2 -> "Chess",
  3 -> "Checkers",
  4 -> "Connect 4",
  5 -> "Sudoku",
  6 -> "8 Queens"
)(number)

def drawers(number: Int): (Array[Array[String]], JFrame, JPanel) => Array[Array[String]] = HashMap( //maps the game number to its drawer function
  1 -> TicTacToeDrawer,
  2 -> ChessDrawer,
  3 -> CheckersDrawer,
  4 -> Connect4Drawer,
  5 -> SudokuDrawer,
  6 -> Queens8Drawer
)(number)

def controllers(number: Int): (String, Array[Array[String]], Int) => Any = HashMap( //maps the game number to its controller function
  1 -> TicTacToeController,
  2 -> ChessController,
  3 -> CheckersController,
  4 -> Connect4Controller,
  5 -> SudokuController,
  6 -> Queens8Controller
)(number)

def initializers(number: Int): () => Array[Array[String]] = HashMap( //maps the game number to its initializer function
  1 -> TicTacToeInitializeGrid _,
  2 -> ChessInitializeGrid _,
  3 -> CheckersInitializeGrid _,
  4 -> Connect4InitializeGrid _,
  5 -> SudokuInitializeGrid _,
  6 -> Queens8InitializeGrid _
)(number)

def switch1Player(player: Int): Int = {
  println("enter your play: ")
  player
}
def switch2Players(player: Int): Int = {
  println("player " + (3 - player) + " turn: ")
  3 - player
}
def invalid1Player(player: Int): Int = {
  println("invalid play! ")
  player
}
def invalid2Players(player: Int): Int = {
  println("invalid play! ")
  3 - player
}