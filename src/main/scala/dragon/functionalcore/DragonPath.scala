package dragon.functionalcore

import dragon.functionalcore.Direction.{East, North, South, West}
import dragon.functionalcore.{Direction, translate}

type DragonPath = List[Point]

object DragonPath:
  def apply(startPoint: Point): DragonPath = List(startPoint)

extension (path: DragonPath)

  def grow(age: Int, length: Int, direction: Direction): DragonPath =

    def newDirections(direction: Direction): (Direction, Direction) =
      direction match
        case North => (West, North)
        case South => (East, South)
        case East  => (East, North)
        case West  => (West, South)

    path.headOption.fold(path): front =>
      if age == 0
      then front.translate(direction, length) :: path
      else
        val (firstDirection, secondDirection) = newDirections(direction)
        path
          .grow(age - 1, length, firstDirection)
          .grow(age - 1, length, secondDirection)

  def lines: List[Line] =
    if path.length < 2 then Nil
    else path.zip(path.tail)