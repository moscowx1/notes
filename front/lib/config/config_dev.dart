import 'config.dart';

Config getDevConfig() {
  return Config(apiConfig: ApiConfig(scheme: "http", host: "localhost", port: 8080));
}
