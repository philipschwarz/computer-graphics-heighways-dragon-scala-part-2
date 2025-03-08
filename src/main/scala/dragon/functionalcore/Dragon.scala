package dragon.functionalcore

case class Dragon(startPoint: Point, age: Int, length: Int, direction: Direction):

  val path: DragonPath = 
    DragonPath(startPoint)
      .grow(age, length, direction)