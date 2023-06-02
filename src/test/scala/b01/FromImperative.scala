package b01

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class FromImperative extends AnyWordSpec with Matchers {
  "multiply by two" should {
    def impByTwo(numbers: ListBuffer[Int]): Unit = {
      for(i <- 0 until numbers.size ) {
        numbers(i) = numbers(i) * 2
      }
    }

    def funByTwo(numbers: List[Int]): List[Int] = numbers.map(_*2)

    "be solved imperatively" in {
      val numbers = ListBuffer(1, 2, 3, 4, 5)
      impByTwo(numbers)

      numbers shouldBe ListBuffer(2, 4, 6, 8, 10)

    }

    "be solved functionally" in {
      val numbers = List(1, 2, 3, 4, 5)
      val result = funByTwo(numbers)

      result shouldBe List(2, 4, 6, 8, 10)

    }
  }

  "divide even by two" should {
    def impDivideByTwo(numbers: List[Int]): List[Int] = {
      val results = new Array[Int](numbers.size)

      var j = 0

      for (i <- 0 until numbers.size) {
        if(numbers(i) % 2 == 0) {
          results(j) = numbers(i) / 2
          j += 1
        }
      }

      results.take(j).toList
    }

    def funDivideByTwo(numbers: List[Int]): List[Int] = numbers.filter(_ % 2 == 0).map(_ / 2)
    def forDivideByTwo(numbers: List[Int]): List[Int] = for {
      n <- numbers
      if n % 2 == 0
    } yield n / 2

    "be solved imperatively" in {
      val numbers = List(1, 2, 3, 4, 5, 6)
      val result = impDivideByTwo(numbers)

      result shouldBe List(1, 2, 3)
    }

    "be solved functionally" in {
      val numbers = List(1, 2, 3, 4, 5, 6)
      val result = funDivideByTwo(numbers)

      result shouldBe List(1, 2, 3)
    }

    "be solved with for" in {
      val numbers = List(1, 2, 3, 4, 5, 6)
      val result = forDivideByTwo(numbers)

      result shouldBe List(1, 2, 3)
    }
  }

  "average" should {
    def impAverage(numbers: List[Int]): Double = {
      var acc = 0

      for( i <- numbers.indices ) {
        acc += numbers(i)
      }

      acc.toDouble / numbers.size
    }

    def funAverage(numbers: List[Int]): Double = numbers.fold(0)(_ + _) / numbers.size


    val cases: Map[String, List[Int] => Double] = Map(
      "be solved imperatively" -> impAverage,
      "be solved functionally" -> funAverage
    )

    checkAll(cases) {
      f =>
        val numbers = List(1, 2, 3, 4, 5)
        val result = f(numbers)

        result shouldBe 3.0
    }
  }

  "add two lists" should {
    def impAddLists(list1: List[Int], list2: List[Int]): List[Int] = {
      val minSize = list1.size.min(list2.size)
      val results = new Array[Int](minSize)

      for ( i <- 0 until minSize)
        results(i) = list1(i) + list2(i)

      results.toList
    }

    def funAddLists(list1: List[Int], list2: List[Int]): List[Int] = list1.zip(list2).map(t => t._1 + t._2)
    def forAddLists(list1: List[Int], list2: List[Int]): List[Int] = for {
      a <- list1
      b <- list2
    } yield a + b

    val cases = Map(
      "be solved imperatively" -> impAddLists _,
      "be solved functionally" -> funAddLists _,
      "be solved in for"       -> funAddLists _

    )

    checkAll2(cases) {
      f =>
        val list1 = List(1, 2, 3)
        val list2 = List(4, 5)
        val result = f(list1, list2)

        result shouldBe List(5, 7)
    }

  }

  "permutations" should {
    def impPermutations(list: List[Int]): List[(Int, Int)] = {
      val nPermutations = list.size * list.size
      val result = new Array[(Int, Int)](nPermutations)
      var k = 0
      for( i <- 0 until list.size )
        for( j <- 0 until list.size) {
          result(k) = (list(i), list(j))
          k += 1
        }

      result.toList
    }

    def funPermutations(list: List[Int]): List[(Int, Int)] = list.flatMap(x => list.map(y => (x, y)))
    def forPermutations(list: List[Int]): List[(Int, Int)] = for {
      x <- list
      y <- list
    } yield (x,y)

    val cases = Map(
      "be solved imperatively" -> impPermutations _,
      "be solved functionally" -> funPermutations _,
      "be solved with for"     -> forPermutations _
    )

    checkAll(cases) {
      f =>
        val numbers = List(1, 2, 3)
        val result = f(numbers)

        result should contain theSameElementsAs List((1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3))
    }

  }

  "max" should {
    def impMax(list: List[Int]): Int = {
      var max = Int.MinValue

      for( i <- 0 until list.size)
        if(list(i) > max)
          max = list(i)

      max
    }

    def funMax(list: List[Int]): Int = {
      @tailrec
      def maxRec(list: List[Int], currentMax: Int): Int = list match{
        case Nil => currentMax
        case head :: tail =>
          val newMax = head.max(currentMax)
          maxRec(tail, newMax)
      }

      maxRec(list, Int.MinValue)
    }

    val cases = Map(
      "be solved imperatively" -> impMax _,
      "be solved functionally" -> funMax _
    )

    checkAll(cases) {
      f =>
        val numbers = List(3, 2, 5, 4, 1)
        val result = f(numbers)

        result shouldBe 5
    }

  }

  "factorial" should {
    def impFactorial(number: Long): Long = {
      var current = 1L

      for( x <- 1L to number )
        current *= x

      current
    }

    def funFactorial(number: Long): Long = {
      require(number >= 0)

      @tailrec
      def factorialRec(number: Long, acc: Long): Long = number match {
        case 0 => 1
        case 1 => acc
        case _ => factorialRec(number - 1, acc*number)
      }

      factorialRec(number, 1)
    }

    val cases = Map(
      "be solved imperatively" -> impFactorial _,
      "be solved functionally" -> funFactorial _
    )

    checkAll(cases) {
      f =>
        f(0) shouldBe 1
        f(1) shouldBe 1
        f(6) shouldBe 720
        f(100000L) shouldBe 0 // Number so big it doesn't fit
    }
  }

  def checkAll[T, U](fs: Map[String, T => U])(check: (T => U) => Unit): Unit = {
    fs.foreach {
      case (name, f) => name in {
        check(f)
      }
    }
  }

  def checkAll2[S, T, U](fs: Map[String, (S, T) => U])(check: ((S, T) => U) => Unit): Unit = {
    fs.foreach {
      case (name, f) => name in {
        check(f)
      }
    }
  }
}
