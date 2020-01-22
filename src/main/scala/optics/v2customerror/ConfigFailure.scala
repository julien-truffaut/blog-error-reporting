package optics.v2customerror

enum ConfigFailure {
  case MissingKey(key: String)
  case InvalidFormat(expectedFormat: String, actual: Config)
}

