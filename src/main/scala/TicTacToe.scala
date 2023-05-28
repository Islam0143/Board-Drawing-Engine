package Games

import java.awt._
import javax.swing._
import scala.collection.immutable.List

def TicTacToeInitializeGrid(): Array[Array[String]] = Array.fill(3, 3)(" ")

def TicTacToeDrawer(grid: Array[Array[String]]): Array[Array[String]] = {
  val frame = new JFrame("Board Drawing Game")
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setSize(500, 500)
  val panel = new JPanel(new GridLayout(3, 3))
  panel.setSize(500, 500)
  val buttonsList: List[List[JButton]] = List.fill(3, 3)(new JButton())
  buttonsList.zipWithIndex.foreach { case (row, i) =>
    row.zipWithIndex.foreach { case (button, j) =>
      button.setText(grid(i)(j))
      button.setFont(new Font("Arial", Font.BOLD, 80))
      button.setBackground(Color.lightGray)
    }
  }
  buttonsList.reverse.foreach(row => row.foreach(panel.add))
  frame.add(panel)
  frame.setVisible(true)
  grid
}

def TicTacToeController(move: String, grid: Array[Array[String]], player:Int): Any = {
  def TicTacToeValidation(move: String, grid: Array[Array[String]]): Any = {
    Option(move).filter(_.length == 2)
      .filter(_ => "123".toList.contains(move.charAt(0)) && "abc".toList.contains(move.charAt(1)))
      .filter(_ => grid(move.charAt(0).asDigit - 1)(map(move.charAt(1))).equals(" "))
      .map(_ => TicTacToeUpdateGrid(move.charAt(0).asDigit - 1, map(move.charAt(1)), grid, player))
      .getOrElse(invalid2PlayersFunction)
  }
  def TicTacToeUpdateGrid(row: Int, col: Int, grid: Array[Array[String]], player: Int): Array[Array[String]] = {
    grid(row).update(col, XOSymbol(player))
    grid
  }
  def XOSymbol(player: Int): String = if(player == 1) "X" else "O"
  
  TicTacToeValidation(move, grid)
}