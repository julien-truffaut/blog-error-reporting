package optics.v3variance

object Example extends App {

  import Config._
  import ConfigFailure._

  def index[V](key: String): Optional[MissingKey, Map[String, V], V] =
    new Optional[MissingKey, Map[String, V], V] {
      def getOrError(from: Map[String, V]): Either[MissingKey, V] =
        from.get(key).toRight(new MissingKey(key))

      def replace(to: V, from: Map[String, V]): Map[String, V] =
        if(from.contains(key)) from + (key -> to)
        else from
    }

  val config: Config = ObjectConfig(Map(
    "john"  -> ObjectConfig(Map(
      "name"  -> StringConfig("John Doe"),
      "age"   -> IntConfig(23)
    )),
    "marie" -> ObjectConfig(Map(
      "name"  -> StringConfig("Marie Acme"),
      "age"   -> StringConfig("forty-three")
    ))
  ))


  def userAge(usrername: String): Optional[ConfigFailure, Config, Int] =
    (obj >>> index(usrername) >>> obj >>> index("age") >>> int)

  println(userAge("john").getOrError(config))
  println(userAge("marie").getOrError(config))
  println(userAge("bob").getOrError(config))


}