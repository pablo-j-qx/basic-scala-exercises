package b03

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FirstClassCitizens extends AnyWordSpec with Matchers {

  // Functions are first class citizens: they have the same right that any other value
  "As first class citizens, functions" should {
    def half(x: Int): Int = x / 2
    def double(x: Int): Int = x * 2
    def add(x: Int, y: Int): Int = x + y

    "be able to be assigned" in {
      var f: Int => Int = half

      f(4) should be (???)

      f = double

      f(4) should be (???)

      // f = add

    }

    "be able to be used as parameter" in {
      def applyToEven(list: List[Int], f: Int => Int): List[Int] = ???

      applyToEven(List(1, 2, 3, 4), half) shouldBe List(1, 1, 3, 2)
      applyToEven(List(1, 2, 3, 4), double) shouldBe List(1, 4, 3, 8)
    }

    "be able to be used as parameter of a map" in {
      List(1, 2, 3, 4).map(double) should be (???)
      List(1, 2, 3, 4).map { x => x*2 } should be  (???)

    }

    "be able to be returned from a function" in {
      def getFindBetterFunction(f: Int => Int): List[Int] => Int = { list: List[Int] =>
        ???
      }

      val findBigger = getFindBetterFunction(identity)
      findBigger(List(-2, -1, 0, 1)) shouldBe 1

      val findBiggerAbsolute = getFindBetterFunction(Math.abs)
      findBiggerAbsolute(List(-2, -1, 0, 1)) shouldBe -2
      
    }

    "be able to be partially applied" in {
      var f: Int => Int = half

      f(4) should be(???)

      f = double

      f(4) should be(???)

      f = add(3, _)

      f(4) should be(???)

    }

    "allow currying" in {
      def curryAdd(x: Int)(y: Int): Int = x + y

      val f: Int => Int = curryAdd(3)

      f(4) should be(???)
    }

    "more curry" in {
      def findBetter(f: Int => Int)(list: List[Int]): Int = ???

      val findBigger: List[Int] => Int = findBetter(identity)
      findBigger(List(-2, -1, 0, 1)) shouldBe 1

      val findBiggerAbsolute: List[Int] => Int = findBetter(Math.abs)
      findBiggerAbsolute(List(-2, -1, 0, 1)) shouldBe -2
    }

    "even more curry" in {
      def repeat(times: Int)(f: Any => Any): Unit = ???

      var counter = 0

      repeat(3) { _ =>
        counter += 1
      }

      counter shouldBe 3


    }


  }

}
