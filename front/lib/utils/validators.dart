import 'package:flutter/material.dart';
import 'package:form_builder_validators/form_builder_validators.dart';

class Validators {
  static FormFieldValidator<T> composeWithSubmited<T>(
      bool submitted, List<FormFieldValidator<T>> validators) {
        return (v) {
          print(submitted);
          if (!submitted) {
            return null;
          }

          return FormBuilderValidators.compose(validators)(v);
        };
  }

  static FormFieldValidator<String> dontStartEndWith(
    String sym, {
    String? errorText,
  }) {
    return (valueCandidate) {
      if (valueCandidate == null) {
        return null;
      }

      errorText ??= 'must not start with \'$sym\'';

      if (valueCandidate.startsWith(sym) || valueCandidate.endsWith(sym)) {
        return errorText;
      }

      return null;
    };
  }
}
