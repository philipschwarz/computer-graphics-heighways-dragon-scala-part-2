package dragon.functionalcore

import dragon.functionalcore.Direction.{East, North, South, West}
import dragon.functionalcore.{Direction, translate}

import scala.annotation.tailrec

type DragonPath = List[Point]

object DragonPath:
  def apply(startPoint: Point, direction: Direction, length: Int): DragonPath =
    val nextPoint = startPoint.translate(direction, amount = length)
    List(startPoint, nextPoint)

extension (path: DragonPath)

  @tailrec
  def grow(age: Int): DragonPath =
    if age == 0 then path
    else (path ++ path.invertedAndRotated).grow(age - 1)

  private def invertedAndRotated: DragonPath =
    val ninetyDegreesClockwise = -Math.PI / 2
    if path.size < 2 then path
    else path
      .init // don't bother rotating the last point, which is the center of rotation
      .reverse // the path that we are rotating is to be trodden in the opposite direction
      .rotate(rotationPoint = path.last,
        rotationAngle = ninetyDegreesClockwise)

  def lines: List[Line] =
    if path.length < 2 then Nil
    else path.zip(path.tail)