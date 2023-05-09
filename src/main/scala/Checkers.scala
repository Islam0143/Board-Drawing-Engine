package Games

import java.awt.*
import javax.swing.*
import scala.collection.immutable.List
import math.*
import scala.util.matching.Regex


def CheckersInitializeGrid(): Array[Array[String]] = {
  Array.fill(8, 8)(" ")
    .updated(0, Array("w\u23FA", " ", "w\u23FA", " ", "w\u23FA", " ", "w\u23FA", " "))
    .updated(1, Array(" ", "w\u23FA", " ", "w\u23FA", " ", "w\u23FA", " ", "w\u23FA"))
    .updated(2, Array("w\u23FA", " ", "w\u23FA", " ", "w\u23FA", " ", "w\u23FA", " "))
    .updated(5, Array(" ", "b\u23FA", " ", "b\u23FA", " ", "b\u23FA", " ", "b\u23FA"))
    .updated(6, Array("b\u23FA", " ", "b\u23FA", " ", "b\u23FA", " ", "b\u23FA", " "))
    .updated(7, Array(" ", "b\u23FA", " ", "b\u23FA", " ", "b\u23FA", " ", "b\u23FA"))
}

def CheckersDrawer(grid: Array[Array[String]], frame: JFrame, panel: JPanel): Array[Array[String]] = {
  val buttonsList: List[List[JButton]] = List.fill(8, 8)(new JButton())
  panel.removeAll()
  panel.setLayout(new GridLayout(8, 8))
  val lettersPanel = new JPanel(new GridLayout(1, 9))
  List("a", "b", "c", "d", "e", "f", "g", "h").foreach {
    letter => lettersPanel.add(new JLabel(letter, SwingConstants.CENTER))
  }
  val numbersPanel = new JPanel(new GridLayout(8, 1))
  List(1, 2, 3, 4, 5, 6, 7, 8).reverse.foreach {
    number => numbersPanel.add(new JLabel(number.toString, SwingConstants.CENTER))
  }
  buttonsList.zipWithIndex.foreach { case (row, i) =>
    row.zipWithIndex.foreach { case (button, j) =>
      button.setText(grid(i)(j).substring(1))
      button.setBackground(if ((i + j) % 2 == 0) Color.GRAY else Color.blue)
      button.setForeground(if (grid(i)(j).charAt(0).equals('w')) Color.WHITE else Color.BLACK)
      button.setOpaque(true)
      button.setBorderPainted(false)
      button.setFont(new Font("Arial Unicode MS", Font.PLAIN, 26))
    }
  }
  buttonsList.reverse.foreach(row => row.foreach(panel.add))
  frame.getContentPane.add(panel, BorderLayout.CENTER)
  frame.add(lettersPanel, BorderLayout.SOUTH)
  frame.add(numbersPanel, BorderLayout.WEST)
  frame.setVisible(true)
  grid
}

def CheckersController(move: String, grid: Array[Array[String]], player: Int): Any = {

  def CheckersUpdateGrid(row1: Int, col1: Int, row2: Int, col2: Int, grid: Array[Array[String]]): Array[Array[String]] = {
    grid(row2).update(col2, grid(row1)(col1))
    grid(row1).update(col1, " ")
    if(abs(row1-row2) == 2)grid((row1+row2)/2).update((col1+col2)/2, " ")
    grid
  }

  def CheckersIsValid(move: String, grid: Array[Array[String]], player: Int, inputPattern: Regex): Boolean = move match {
    case inputPattern(row1, col1, row2, col2) =>
      (grid(row1(0).asDigit - 1)(map(col1(0))), player, grid(row2(0).asDigit - 1)(map(col2(0))), row2(0) - row1(0), map(col2(0)) - map(col1(0))) match {
        case ("w\u23FA", 1, " ", 1, 1 | -1) | ("b\u23FA", 2, " ", -1, -1 | 1) => true
        case ("w\u23FA", 1, " ", 2, 2 | -2) | ("b\u23FA", 2, " ", -2, -2 | 2) =>
          (grid(min(row2(0).asDigit, row1(0).asDigit))(min(map(col2(0)), map(col1(0))) + 1), player) match {
            case ("w\u23FA", 2) | ("b\u23FA", 1) => true
            case _ => false
          }
        case _ => false
          }
    case _ => false
  }
  if(CheckersIsValid(move, grid, player, """([1-9])([a-i]) ([1-9])([a-i])""".r))
    CheckersUpdateGrid(move(0).asDigit-1, map(move(1)), move(3).asDigit-1, map(move(4)), grid)
  else invalid2Players(player)
}