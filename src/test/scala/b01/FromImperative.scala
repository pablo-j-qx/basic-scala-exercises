package b01

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ListBuffer

class FromImperative extends AnyWordSpec with Matchers {
  "multiply by two" should {
    def impByTwo(numbers: ListBuffer[Int]): Unit = {
      for(i <- 0 until numbers.size ) {
        numbers(i) = numbers(i) * 2
      }
    }

    def funByTwo(numbers: ListBuffer[Int]): Unit = ???

    "be solved imperatively" in {
      val numbers = ListBuffer(1, 2, 3, 4, 5)
      impByTwo(numbers)

      numbers shouldBe ListBuffer(2, 4, 6, 8, 10)

    }

    "be solved functionally" in {
      val numbers = ListBuffer(1, 2, 3, 4, 5)
      funByTwo(numbers)

      numbers shouldBe ListBuffer(2, 4, 6, 8, 10)

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

    def funDivideByTwo(numbers: List[Int]): List[Int] = ???

    "be solved imperatively" in {
      val numbers = List(1, 2, 3, 4, 5, 6)
      val result = impDivideByTwo(numbers)

      result shouldBe List(1, 2, 3)
    }

    "be solved functionally" in {
      val numbers = List(1, 2, 3, 4, 5)
      funDivideByTwo(numbers)

      numbers shouldBe List(1, 2, 3)
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

    def funAverage(numbers: List[Int]): Double = ???


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

    def funAddLists(list1: List[Int], list2: List[Int]): List[Int] = ???

    val cases = Map(
      "be solved imperatively" -> impAddLists _,
      "be solved functionally" -> funAddLists _
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

    def funPermutations(list: List[Int]): List[(Int, Int)] = ???

    val cases = Map(
      "be solved imperatively" -> impPermutations _,
      "be solved functionally" -> funPermutations _
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

    def funMax(list: List[Int]): Int = ???

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

    def funFactorial(number: Long): Long = ???

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
