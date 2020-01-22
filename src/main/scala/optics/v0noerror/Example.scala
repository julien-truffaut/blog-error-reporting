package optics.v0noerror

import optics.User

object Example {

  val email: Optional[User, String] =
    new Optional[User, String] {
      def getOption(from: User): Option[String] =
        from.email

      def replace(to: String, from: User): User =
        getOption(from).fold(from)(_ => from.copy(email = Some(to)))
    }

  val john = User("John Doe", 23, Some("john@foo.com"))

  email.getOption(john)
  email.replace("john@doe.com", john)



  def index[K, V](key: K): Optional[Map[K, V], V] =
    new Optional[Map[K, V], V] {
      def getOption(from: Map[K, V]): Option[V] =
        from.get(key)

      def replace(to: V, from: Map[K, V]): Map[K, V] =
        if(from.contains(key)) from + (key -> to)
        else from
    }

  val users: Map[String, Int] = Map("john" -> 23, "marie" -> 34)

  index("john").getOption(users)
  index("bob").getOption(users)
  index("john").replace(20, users)
  index("bob").replace(20, users)

  val users2: Map[String, User] = Map(
    "john"  -> User("John Doe"  , 23, Some("john@foo.com")),
    "marie" -> User("Marie Acme", 34, None)
  )

  (index("john" ) >>> email).getOption(users2)
  (index("bob")   >>> email).getOption(users2)
  (index("john") >>> email).replace("john@gmail.com", users2)

}
