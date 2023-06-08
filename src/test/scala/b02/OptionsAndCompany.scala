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
      def getAllEmails(organization: Organization): Seq[String] = ???

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
      List(1, 2, 3).isEmpty should be (???)
      List().isEmpty should be (???)
      Some(2).isEmpty should be (???)
      None.isEmpty should be (???)
    }

    "map" in {
      List(1, 2, 3).map(_ * 2) shouldBe ???
      Some(2).map(_ * 2) shouldBe ???
      none.map(_ * 2) shouldBe ???
    }
    "flatten" in {
      List(List(1, 2, 3), List(4, 5, 6)).flatten shouldBe ???
      Some(Some(2)).flatten shouldBe ???
      Some(none).flatten shouldBe ???
      val noneOption: Option[Option[Int]] = None
      noneOption.flatten shouldBe ???
    }

    "filter" in {
      def isEven(x: Int): Boolean = x % 2 == 0

      List(1, 2, 3, 4, 5).filter(isEven) shouldBe ???
      Some(2).filter(isEven) shouldBe ???
      Some(1).filter(isEven) shouldBe ???
      None.filter(isEven) shouldBe ???

    }

    "find" in {
      def isEven(x: Int): Boolean = x % 2 == 0

      List(1, 2, 3, 4, 5).find(isEven) shouldBe ???
      List(1, 3, 5).find(isEven) shouldBe ???
      Some(2).find(isEven) shouldBe ???
      Some(1).find(isEven) shouldBe ???
      None.find(isEven) shouldBe ???
    }

    "fold" in {
      List(1, 2, 3, 4).fold(0)(_ + _) should be (???)

      List(1, 2, 3, 4).fold(0){ case (acc, value) =>
        acc + value
      } should be (???)

      def add(a: Int, b: Int): Int = a + b

      List(1, 2, 3, 4).fold(0)(add) should be (???)

      Some(2).fold(0)(8 / _) should be (???)
      None.fold(0)(8 / _) should be (???)

      def whatDoIHave(x: Option[Int]): String = x.fold("nothing")(€ => s"$€ euros")
      s"I have ${whatDoIHave(Some(2))}." shouldBe ???
      s"I have ${whatDoIHave(None)}." shouldBe ???

    }
  }

}
