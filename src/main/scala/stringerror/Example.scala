package stringerror

import Optional.index

object Example {

  val users: Map[String, Map[String, String]] =
    Map(
      "john"  -> Map("name" -> "John Doe"  , "email" -> "john@foo.com"),
      "marie" -> Map("name" -> "Marie Acme")
    )

    (index("john" ) >>> index("email")).getOrError(users)
    (index("marie") >>> index("email")).getOrError(users)
    (index("bob") >>> index("name")).getOrError(users)
    (index("john") >>> index("email")).replace("john@gmail.com", users)

}
