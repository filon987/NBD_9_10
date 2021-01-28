object Zadanie9 {
  class Container[A] {
    private var _value:A = null.asInstanceOf[A]

    def this(value:A){
      this()
      this._value = value
    }

    def getContent(): A= _value

    def applyFunction(f: A => Any): Any = f(_value)

  }


  trait Maybe[A] {
    def applyFunction(func: A => Any) : Maybe[_]

    def getOrElse[B](value: B) : Any

  }

  class No extends Maybe[Nothing] {
    override def applyFunction(func: Nothing => Any): Maybe[_] = new No()
    override def getOrElse[B](value: B): B = value
  }

  class Yes[A](val value:A) extends Maybe[A] {
    override def applyFunction(func: A => Any): Maybe[_] = new Yes[Any](func(value))
    override def getOrElse[B](otherVal: B): A = value
  }

  def main(args: Array[String]): Unit = {

    println("Zadanie 1")

    val conNumber = new Container(7)
    def multiplyByTwo(number: Int): Int = 2 * number

    println("getContent(): " + conNumber.getContent())
    println("applyFunc(math): " + conNumber.applyFunction(multiplyByTwo))

    val conString = new Container("Hello World")
    def lowerCase(n: String): String = n.toLowerCase()
    println("getContent(): " + conString.getContent())
    println("applyFunc(lowerCase): " + conString.applyFunction(lowerCase))


    println()
    println("Zadanie 2")

    val y = new Yes(7)
    val n = new No()

    println("y: " + y.isInstanceOf[Maybe[_]])
    println("n: " + n.isInstanceOf[Maybe[_]])


    println()
    println("Zadanie 3")

    println("y: " + y.applyFunction(multiplyByTwo))
    println("n: " + n.applyFunction(multiplyByTwo))


    println()
    println("Zadanie 4")

    println(n.getOrElse(y.getOrElse(new No())))
    println(y.getOrElse(n.getOrElse(new Yes())))
  }
}