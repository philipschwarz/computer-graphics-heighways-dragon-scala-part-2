package dragon.functionalcore

import dragon.functionalcore.translate

import scala.annotation.tailrec

type DragonPath = List[Point]

val ninetyDegreesClockwise = -Math.PI / 2

object DragonPath:

  def apply(startPoint: Point, direction: Direction, length: Int): DragonPath =
    val nextPoint = startPoint.translate(direction, amount = length)
    List(nextPoint, startPoint)

extension (path: DragonPath)

  @tailrec
  def grow(age: Int): DragonPath =
    if age == 0 || path.size < 2 then path
    else path.plusRotatedCopy.grow(age - 1)

  // simpler to read but less efficient because it uses append
  private def plusRotatedCopy1 =
    path.reverse.rotate(rotationCentre = path.head, angle = ninetyDegreesClockwise)
      ++ path

  // harder to read but more efficient because it uses a left fold
  private def plusRotatedCopy: DragonPath =
    path.foldLeft(path): (growingPath, point) =>
      point.rotate(rotationCentre = path.head, angle = ninetyDegreesClockwise)
        :: growingPath

  def lines: List[Line] =
    if path.length < 2 then Nil
    else path.zip(path.tail)