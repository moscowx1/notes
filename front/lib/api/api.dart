import 'package:front/api/login_req.dart';
import 'package:front/config/config.dart';
import 'package:http/browser_client.dart';
import 'package:http/http.dart' as http;

class Api {
  final ApiConfig apiConfig;
  late final http.Client client;

  Api(this.apiConfig) {
    client = http.Client();

    if (client is BrowserClient) {
      (client as BrowserClient).withCredentials = true;
    }
  }

  Uri _buildUri(String path) {
    return Uri(
        scheme: apiConfig.scheme,
        host: apiConfig.host,
        port: apiConfig.port,
        path: path);
  }

  login(LoginReq req) async {
    try {
      var uri = _buildUri('/auth/sign-in');
      var headers = <String, String>{"Content-Type": "application/json"};

      var resp = await http.post(uri, headers: headers, body: req);
      return resp;
    } on Error catch (_) {
      return null;
    }
  }
}
