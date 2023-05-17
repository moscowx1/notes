class ApiConfig {
  final String scheme;
  final String host;
  final int? port;

  ApiConfig({required this.scheme, required this.host, this.port});
}

class Config {
  final ApiConfig apiConfig;

  Config({required this.apiConfig});
}
