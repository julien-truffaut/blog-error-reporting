package optics.v1stringerror

import optics.User

object Example extends App {

  val email = new Optional[User, String] {
    def getOrError(from: User): Either[String, String] =
      from.email.toRight("email is missing")

    def replace(to: String, from: User): User =
      if(from.email.isDefined) from.copy(email = Some(to)) else from
  }

  def index[K, V](key: K): Optional[Map[K, V], V] =
    new Optional[Map[K, V], V] {
      def getOrError(from: Map[K, V]): Either[String, V] =
        from.get(key).toRight(s"Key $key is missing")

      def replace(to: V, from: Map[K, V]): Map[K, V] =
        if(from.contains(key)) from + (key -> to)
        else from
    }

  val users: Map[String, User] = Map(
    "john"  -> User("John Doe"  , 23, Some("john@foo.com")),
    "marie" -> User("Marie Acme", 34, None)
  )

  println((index("john" ) >>> email).getOrError(users))
  println((index("marie") >>> email).getOrError(users))
  println((index("bob")   >>> email).getOrError(users))
  println((index("john")  >>> email).replace("john@gmail.com", users))

}
