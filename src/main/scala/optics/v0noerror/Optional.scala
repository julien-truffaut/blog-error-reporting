package optics.v0noerror

import scala.annotation.alpha

trait Optional[From, To] { self =>
  def getOption(from: From): Option[To]
  def replace(to: To, from: From): From

  @alpha("andThen")
  def >>>[Next](other: Optional[To, Next]): Optional[From, Next] =
    new Optional[From, Next] {
      def getOption(from: From): Option[Next] =
        self.getOption(from).flatMap(other.getOption)

      def replace(to: Next, from: From): From =
        self.getOption(from)
          .map(other.replace(to, _))
          .fold(from)(self.replace(_, from))
    }
}