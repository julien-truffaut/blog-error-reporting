package optics.v2customerror


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

//  def property(key: String): Optional[ConfigFailure, Config, Config] =
//    obj >>> index(key)

  println(int.getOrError(IntConfig(12)))
  println(int.getOrError(StringConfig("hello")))
  println(index("users").getOrError(Map.empty))

}