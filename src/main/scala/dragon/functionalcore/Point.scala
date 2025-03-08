package dragon.functionalcore

import matr.{Matrix, MatrixFactory}
// Bring bundled implementations in scope
import matr.MatrBundle.given
import matr.TupleSupport.given
import matr.dflt.DefaultMatrixFactory.given
import matr.dflt.DefaultMatrixOps.given
import matr.std.StandardOps.given

import dragon.functionalcore.Direction
import dragon.functionalcore.Direction.{East, North, South, West}

case class Point(x: Float, y: Float)

extension (p: Point)

  def deviceCoords(panelHeight: Int): (Int, Int) =
    (Math.round(p.x), panelHeight - Math.round(p.y))

  def translate(direction: Direction, amount: Float): Point =
    direction match
      case North => Point(p.x, p.y + amount)
      case South => Point(p.x, p.y - amount)
      case East  => Point(p.x + amount, p.y)
      case West  => Point(p.x - amount, p.y)

  def rotate(rotationPoint: Point, rotationAngle: Double): Point =
    val cos = math.cos(rotationAngle).toFloat
    val sin = math.sin(rotationAngle).toFloat
    val rp = rotationPoint
    val pointMatrix = MatrixFactory[1, 3, Float].rowMajor(p.x, p.y, 1.0)
    val rotationMatrix = MatrixFactory[3, 3, Float].fromTuple(
      (cos, sin, 0.0f),
      (-sin, cos, 0.0f),
      (-rp.x * cos + rp.y * sin + rp.x, -rp.x * sin - rp.y * cos + rp.y, 1.0f)
    )
    val resultMatrix: Matrix[1, 3, Float] = pointMatrix dot rotationMatrix
    Point(resultMatrix(0, 0), resultMatrix(0, 1))

extension (points: List[Point])
  def rotate(rotationPoint: Point, rotationAngle: Double) : List[Point] =
    points.map(point => point.rotate(rotationPoint, rotationAngle))    