package optics.v3variance

import ConfigFailure.InvalidFormat

sealed trait Config

object Config {
  case class IntConfig(value: Int) extends Config
  case class StringConfig(value: String) extends Config
  case class ObjectConfig(value: Map[String, Config]) extends Config

  val int: Optional[InvalidFormat, Config, Int] = new Optional[InvalidFormat, Config, Int] {
    def getOrError(from: Config): Either[InvalidFormat, Int] =
      from match {
        case IntConfig(x) => Right(x)
        case other        => Left(new InvalidFormat("Int", other))
      }
    def replace(to: Int, from: Config): Config =
      from match {
        case IntConfig(_) => IntConfig(to)
        case _            => from
      }
  }

  val str: Optional[InvalidFormat, Config, String] = new Optional[InvalidFormat, Config, String] {
    def getOrError(from: Config): Either[InvalidFormat, String] =
      from match {
        case StringConfig(x) => Right(x)
        case other           => Left(new InvalidFormat("String", other))
      }
    def replace(to: String, from: Config): Config =
      from match {
        case StringConfig(_) => StringConfig(to)
        case _               => from
      }
  }

  val obj: Optional[InvalidFormat, Config, Map[String, Config]] = new Optional[InvalidFormat, Config, Map[String, Config]] {
    def getOrError(from: Config): Either[InvalidFormat, Map[String, Config]] =
      from match {
        case ObjectConfig(x) => Right(x)
        case other           => Left(new InvalidFormat("Object", other))
      }
    def replace(to: Map[String, Config], from: Config): Config =
      from match {
        case ObjectConfig(_) => ObjectConfig(to)
        case _               => from
      }
  }
}

