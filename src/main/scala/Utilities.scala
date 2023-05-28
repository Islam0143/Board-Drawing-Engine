package Games
import java.awt.*
import javax.swing.JFrame
import scala.collection.immutable.HashMap

def createCircleIcon(size: Int, color: Color): java.awt.Image = {
  val image = new java.awt.image.BufferedImage(size, size, java.awt.image.BufferedImage.TYPE_INT_ARGB)
  val g = image.getGraphics.asInstanceOf[Graphics2D]
  g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  g.setColor(color)
  g.fillOval(0, 0, size, size)
  g.dispose()
  image
}

def map(ch: Char): Int = "abcdefghi".toList.indexOf(ch)

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
def drawers(number: Int): Array[Array[String]] => Array[Array[String]] = HashMap( //maps the game number to its drawer function
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
  println("Enter your play: ")
  player
}
def switch2Players(player: Int): Int = {
  println("Player " + (3 - player) + " turn: ")
  3 - player
}
def invalid1PlayerFunction(player: Int): Int = {
  println("Invalid play! ")
  println("Enter your play: ")
  player
}
def invalid2PlayersFunction(player: Int): Int = {
  println("Invalid play! ")
  println("Player " + player + " turn: ")
  player
}
def notSolvableFunction(player: Int): Int = {
  println("Puzzle is not solvable! ")
  println("Enter your play: ")
  player
}