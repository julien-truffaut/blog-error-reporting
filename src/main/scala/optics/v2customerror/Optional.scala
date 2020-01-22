package optics.v2customerror

import scala.annotation.alpha

trait Optional[Error, From, To] { self =>
  def getOrError(from: From): Either[Error, To]
  def replace(to: To, from: From): From

  def getOption(from: From): Option[To] =
    getOrError(from).toOption

  @alpha("andThen")
  def >>>[Next](other: Optional[Error, To, Next]): Optional[Error, From, Next] =
    new Optional[Error, From, Next] {
      def getOrError(from: From):  Either[Error, Next] =
        self.getOrError(from).flatMap(other.getOrError)

      def replace(to: Next, from: From): From =
        self.getOrError(from)
          .map(other.replace(to, _))
          .fold(_ => from, self.replace(_, from))
    }
}

object Optional {

  def index[K, V](key: K): Optional[String, Map[K, V], V] =
    new Optional[String, Map[K, V], V] {
      def getOrError(from: Map[K, V]): Either[String, V] =
        from.get(key).toRight(s"Key $key is missing")

      def replace(to: V, from: Map[K, V]): Map[K, V] =
        if(from.contains(key)) from + (key -> to)
        else from
    }

}
