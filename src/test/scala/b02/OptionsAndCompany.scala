package b02

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ListBuffer

class OptionsAndCompany extends AnyWordSpec with Matchers {

  "Person tests" when {
    "written like Java" should {
      case class ContactInformation(phone: String, email: String)
      case class Person(name: String, contactInformation: ContactInformation)
      case class Department(boss: Person, employees: Seq[Person])
      case class Organization(departments: Seq[Department])

      val boss1 = Person("Angela", ContactInformation("123", null))
      val employees1 = Seq(
        Person("Oscar", null),
        Person("Kevin", ContactInformation(null, "kevin@dm.com"))
      )

      val dept1 = Department(boss1, employees1)

      val employees2 = Seq(
        Person("Jim", null),
        Person("Dwight", ContactInformation("456", "dwight@dm.com")),
        Person("Stanley", ContactInformation("789", null)),
        Person("Phyllys", ContactInformation(null, "phyllys@dm.com"))
      )

      val dept2 = Department(null, employees2)

      val organization = Organization(Seq(dept1, dept2))

      // Now the function to be tested
      def getAllEmails(organization: Organization): Seq[String] = {

        def getPersonEmail(person: Person): String = {
          if(person == null)
            null
          else if(person.contactInformation == null)
            null
          else
            person.contactInformation.email
        }

        val emails = ListBuffer.empty[String]
        for ( i <- organization.departments.indices) {
          val department = organization.departments(i)

          val bossEmail = getPersonEmail(department.boss)
          if(bossEmail != null)
            emails.addOne(bossEmail)

          for ( j <- department.employees.indices ) {
            val email = getPersonEmail(department.employees(j))
            if(email != null)
              emails.addOne(email)
          }
        }

        emails.toList
      }

      "get all availabe emails" in {
        getAllEmails(organization) should contain theSameElementsAs List(
          "kevin@dm.com",
          "dwight@dm.com",
          "phyllys@dm.com"
        )
      }
    }

    "written like a Scala chad" should {
      case class ContactInformation(phone: Option[String], email: Option[String])
      case class Person(name: String, contactInformation: Option[ContactInformation])
      case class Department(boss: Option[Person], employees: Seq[Person])
      case class Organization(departments: Seq[Department])

      val boss1 = Person("Angela", Some(ContactInformation(Some("123"), None)))
      val employees1 = Seq(
        Person("Oscar", None),
        Person("Kevin", Some(ContactInformation(None, Some("kevin@dm.com"))))
      )

      val dept1 = Department(Some(boss1), employees1)

      val employees2 = Seq(
        Person("Jim", None),
        Person("Dwight", Some(ContactInformation(Some("456"), Some("dwight@dm.com")))),
        Person("Stanley", Some(ContactInformation(Some("789"), None))),
        Person("Phyllys", Some(ContactInformation(None, Some("phyllys@dm.com"))))
      )

      val dept2 = Department(None, employees2)

      val organization = Organization(Seq(dept1, dept2))

      // Now the function to be tested
      def getAllEmails(organization: Organization): Seq[String] = {
        def getPersonEmail(person: Person): Option[String] = for {
          contact <- person.contactInformation
          email <- contact.email
        } yield email

        (for {
          department <- organization.departments
          boss = department.boss
          person <- boss.toList ++ department.employees
        } yield getPersonEmail(person)).flatMap(_.toList)
      }

      "get all availabe emails" in {
        getAllEmails(organization) should contain theSameElementsAs List(
          "kevin@dm.com",
          "dwight@dm.com",
          "phyllys@dm.com"
        )
      }
    }

  }

  "Option and other monads operations" should {
    // We use this val as equivalent to None when we need the compiler to be aware of the types
    val none: Option[Int] = None

    "empty" in {
      List(1, 2, 3).isEmpty should be (false)
      List().isEmpty should be (true)
      Some(2).isEmpty should be (false)
      None.isEmpty should be (true)
    }

    "map" in {
      List(1, 2, 3).map(_ * 2) shouldBe List(2, 4, 6)
      Some(2).map(_ * 2) shouldBe Some(4)
      none.map(_ * 2) shouldBe None
    }
    "flatten" in {
      List(List(1, 2, 3), List(4, 5, 6)).flatten shouldBe List(1, 2, 3, 4, 5, 6)
      Some(Some(2)).flatten shouldBe Some(2)
      Some(none).flatten shouldBe None
      val noneOption: Option[Option[Int]] = None
      noneOption.flatten shouldBe None
    }

    "filter" in {
      def isEven(x: Int): Boolean = x % 2 == 0

      List(1, 2, 3, 4, 5).filter(isEven) shouldBe List(2, 4)
      Some(2).filter(isEven) shouldBe Some(2)
      Some(1).filter(isEven) shouldBe None
      None.filter(isEven) shouldBe None

    }

    "find" in {
      def isEven(x: Int): Boolean = x % 2 == 0

      List(1, 2, 3, 4, 5).find(isEven) shouldBe Some(2)
      List(1, 3, 5).find(isEven) shouldBe None
      Some(2).find(isEven) shouldBe Some(2)
      Some(1).find(isEven) shouldBe None
      None.find(isEven) shouldBe None
    }

    "fold" in {
      List(1, 2, 3, 4).fold(0)(_ + _) should be (10)

      List(1, 2, 3, 4).fold(0){ case (acc, value) =>
        acc + value
      } should be (10)

      def add(a: Int, b: Int): Int = a + b

      List(1, 2, 3, 4).fold(0)(add) should be (10)

      def divideEight(x: Int): Int = 8 / x
      Some(2).fold(0)(divideEight) should be (4)
      None.fold(0)(divideEight) should be (0)

      def whatDoIHave(x: Option[Int]): String = x.fold("nothing"){e => s"$e euros"}
      s"I have ${whatDoIHave(Some(2))}." shouldBe "I have 2 euros."
      s"I have ${whatDoIHave(None)}." shouldBe "I have nothing."

    }
  }

}
