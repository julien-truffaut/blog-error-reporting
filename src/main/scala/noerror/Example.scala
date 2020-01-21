package noerror

import Optional.index

object Example {

  val users: Map[String, Int] = Map("john" -> 23, "marie" -> 34)

  index("john").getOption(users)
  index("bob").getOption(users)
  index("john").replace(20, users)
  index("bob").replace(20, users)

  val users2: Map[String, Map[String, String]] =
    Map(
      "john"  -> Map("name" -> "John Doe"  , "email" -> "john@foo.com"),
      "marie" -> Map("name" -> "Marie Acme")
    )

    (index("john" ) >>> index("email")).getOption(users2)
    (index("marie") >>> index("email")).getOption(users2)
    (index("bob") >>> index("name")).getOption(users2)
    (index("john") >>> index("email")).replace("john@gmail.com", users2)

}
