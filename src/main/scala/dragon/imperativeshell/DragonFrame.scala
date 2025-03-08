package dragon.imperativeshell

import java.awt.Color
import javax.swing.{JFrame, WindowConstants}

def displayDragonFrame(): Unit =
  val (gold, green) = (Color(255, 215, 0), Color(0, 128, 0))
  val panel = DragonPanel(lineColour = gold, backgroundColour = green)
  JFrame.setDefaultLookAndFeelDecorated(true)
  val frame = new JFrame("Heighway's Dragon")
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setSize(600,600)
  frame.add(panel)
  frame.setVisible(true)