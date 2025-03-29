package dragon.functionalcore

import matr.{Matrix, MatrixFactory}
// Bring bundled implementations in scope
import dragon.functionalcore.Direction.{East, North, South, West}
import matr.TupleSupport.given
import matr.dflt.DefaultMatrixFactory.given
import matr.dflt.DefaultMatrixOps.given
import matr.std.StandardOps.given

case class Point(x: Float, y: Float)

type Radians = Double

extension (p: Point)

  def deviceCoords(panelHeight: Int): (Int, Int) =
    (Math.round(p.x), panelHeight - Math.round(p.y))

  def translate(direction: Direction, amount: Float): Point =
    direction match
      case North => Point(p.x, p.y + amount)
      case South => Point(p.x, p.y - amount)
      case East  => Point(p.x + amount, p.y)
      case West  => Point(p.x - amount, p.y)

  def rotate(rotationCentre: Point, angle: Radians): Point =
    val (c, ϕ) = (rotationCentre, angle)
    val (cosϕ, sinϕ) = (math.cos(ϕ).toFloat, math.sin(ϕ).toFloat)
    val rotationMatrix: Matrix[3,3,Float] = MatrixFactory[3, 3, Float].fromTuple(
      (              cosϕ,                             sinϕ,               0f),
      (             -sinϕ,                             cosϕ,               0f),
      (-c.x * cosϕ + c.y * sinϕ + c.x,   -c.x * sinϕ - c.y * cosϕ + c.y,   1f)
    )
    val rowVector: Matrix[1, 3, Float] = MatrixFactory[1, 3, Float].rowMajor(p.x, p.y, 1f)
    val rotatedRowVector: Matrix[1, 3, Float] = rowVector dot rotationMatrix
    val (x, y) = (rotatedRowVector(0, 0), rotatedRowVector(0, 1))
    Point(x, y)


extension (points: List[Point])
  def rotate(rotationCentre: Point, angle: Radians) : List[Point] =
    points.map(point => point.rotate(rotationCentre, angle))    