package part1.errorhandling

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is invalid.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[List[String], Person] = {
    val (en, ea) = (mkName(name), mkAge(age))
    (en, ea) match {
      case (Right(n), Right(a)) => Right(Person(n, a))
      case (Left(n), Left(a)) => Left(List(n, a))
      case _ => en.map2(ea)(Person(_, _)) match {
        case Left(v) => Left(List(v))
        case _ => Left(List("this never match because of higher patterns"))
      }
    }
  }

}