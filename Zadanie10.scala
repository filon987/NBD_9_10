object Zadanie10 {
  def nextPairGenerator: Iterator[(Int, Int)] =
    for {
      dividend <- Iterator.from(0)
      divider <- 1 until dividend + 1
      if dividend % divider == 0
    } yield(dividend, divider)

  trait Maybe[A] {
    def applyFunction(func: A => Any) : Maybe[_]

    def getOrElse[B](value: B) : Any

    def map[B](fun: A => B): Maybe[_]

    def flatMap[B](fun: A => Maybe[_]): Maybe[_]

  }

  class No extends Maybe[Nothing] {
    override def applyFunction(func: Nothing => Any): Maybe[_] = new No()

    override def getOrElse[B](value: B): B = value

    override def map[B](fun: Nothing => B): Maybe[_] = new No()

    override def flatMap[B](fun: Nothing => Maybe[_]): Maybe[_] = new No()
  }

  class Yes[A](val value:A) extends Maybe[A] {
    override def map[B](fun: A => B): Maybe[B] = new Yes(fun(value))

    override def flatMap[B](fun: A => Maybe[_]): Maybe[_] = fun(value)

    override def applyFunction(func: A => Any): Maybe[_] = new Yes[Any](func(value))

    override def getOrElse[B](otherVal: B): A = value
  }

  def main(args: Array[String]): Unit = {
    println("Zadanie 1")

    var list = List[(Int, Int)]()
    val pair = nextPairGenerator.buffered

    for ( i <- 0 until 20) {
      list = list ::: List(pair.next())
    }

    print(list)


    println()
    println("Zadanie 2")

    val map = (list : List[Int]) => list.map(value => value + 1)
    val flatMap = (list : List[Int]) => new Yes(list.map(value => value + 1))
    val no = new No()
    val yes = new Yes(List(5, 5, 7))

    println(no.map(map).getOrElse(0))
    println(no.map(flatMap).getOrElse(0))

    println(yes.map(map).getOrElse(0))
    println(yes.map(flatMap).getOrElse(0))
  }
}