import 'package:flutter/foundation.dart';
import 'package:flutter_simple_dependency_injection/injector.dart';
import 'package:front/api/api.dart';
import 'package:front/config/config.dart';

import 'config/config_dev.dart';

class InjectorCreater {
  Injector initialise(Injector injector) {
    if (kDebugMode) {
      injector.map((_) => getDevConfig());
    } else {
      throw UnimplementedError('Configuration for current mode not set');
    }

    injector.map((injector) => Api(injector.get<Config>().apiConfig));

    return injector;
  }
}
