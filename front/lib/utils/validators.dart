import 'package:flutter/material.dart';

class Validators {
  static FormFieldValidator<T> _composeWithSubmited<T>(
      bool submitted, List<FormFieldValidator<T>> validators,) {
    return (value) {
      if (!submitted) {
        return null;
      }

      return Validators.compose(validators)(value);
    };
  }

static FormFieldValidator<T> composeWithSubmited<T>(
    bool submitted, List<FormFieldValidator<T>> validators,) {
  return (value) {
    final res = _composeWithSubmited(submitted, validators)(value);
    return res;
  };
}
  static FormFieldValidator<T> compose<T>(
    List<FormFieldValidator<T>> validators,
  ) {
    return (value) {
      for (var validator in validators) {
        final validatorResult = validator(value);
        if (validatorResult != null) {
          return validatorResult;
        }
      }
      return null;
    };
  }

  static FormFieldValidator<String> lengthAround(int min, int max) {
    assert(max > min);
    return Validators.compose([
      minLength(min),
      maxLength(max),
    ]);
  }

  static FormFieldValidator<String> minLength(int length) {
    assert(length >= 0);
    return (value) {
      if (value == null || value.length < length) {
        return 'length must be more than $length';
      }
      return null;
    };
  }

  static FormFieldValidator<String> maxLength(int length) {
    assert(length > 0);
    return (value) {
      if (value == null || value.length < length) {
        return null;
      }

      return 'length must be less than $length';
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
