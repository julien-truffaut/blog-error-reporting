package optics.v3variance

enum ConfigFailure {
  case MissingKey(key: String)
  case InvalidFormat(expectedFormat: String, actual: Config)
}
