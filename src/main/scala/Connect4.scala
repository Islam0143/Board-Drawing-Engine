package Games

import java.awt._
import javax.swing._
import scala.collection.immutable.List


def Connect4InitializeGrid(): Array[Array[String]] = Array.fill(6, 7)(" ")

def Connect4Drawer(grid: Array[Array[String]], frame: JFrame, panel: JPanel): Array[Array[String]] = {
  panel.removeAll()
  frame.setSize(550, 500)
  panel.setSize(550, 500)
  panel.setLayout(new GridLayout(6, 7))
  panel.setBackground(Color.BLUE)
  val buttonsList: List[List[JButton]] = List.fill(6, 7)(new JButton())
  buttonsList.zipWithIndex.foreach { case (row, i) =>
    row.zipWithIndex.foreach { case (button, j) =>
      button.setOpaque(false)
      button.setContentAreaFilled(false)
      button.setBorderPainted(false)
      button.setIcon(new ImageIcon(createCircleIcon(70, grid(i)(j) match {
        case "r" => Color.RED
        case "y" => Color.YELLOW
        case _ => Color.WHITE
      })))
    }
  }
  buttonsList.reverse.foreach(row => row.foreach(panel.add))
  frame.setVisible(true)
  grid
}

def Connect4Controller(move: String, grid: Array[Array[String]], player: Int): Any = {
  def Connect4Validation(move: String, grid: Array[Array[String]]): Int => Any = {
    Option(move).filter(_.length == 1)
      .filter(_ => "abcdefg".toList.contains(move.charAt(0)))
      .filter(_ => playsInCol(grid.map(row => row(map(move.charAt(0)))).toList) < 6)
      .map(_ => Connect4UpdateGrid(playsInCol(grid.map(row => row(map(move.charAt(0)))).toList), map(move.charAt(0)), grid, _))
      .getOrElse(invalid2Players)
  }
  def Connect4UpdateGrid(row: Int, col: Int, grid: Array[Array[String]], player: Int): Array[Array[String]] = {
    grid(row).update(col, Color(player))
    grid
  }
  def playsInCol(column: List[String]): Int =
    column.takeWhile(cell => !cell.equals(" ")).length

  def Color(player: Int): String = if (player == 1) "r" else "y"

  Connect4Validation(move, grid)(player)
}