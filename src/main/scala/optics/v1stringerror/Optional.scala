package optics.v1stringerror

import scala.annotation.alpha

trait Optional[From, To] { self =>
  def getOrError(from: From): Either[String, To]
  def replace(to: To, from: From): From

  def getOption(from: From): Option[To] =
    getOrError(from).toOption

  @alpha("andThen")
  def >>>[Next](other: Optional[To, Next]): Optional[From, Next] =
    new Optional[From, Next] {
      def getOrError(from: From):  Either[String, Next] =
        self.getOrError(from).flatMap(other.getOrError)

      def replace(to: Next, from: From): From =
        self.getOrError(from)
          .map(other.replace(to, _))
          .fold(_ => from, self.replace(_, from))
    }
}