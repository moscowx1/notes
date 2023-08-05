import { AbstractControl, ValidationErrors, ValidatorFn } from '@angular/forms';

export class CustomValidators {
  static MatchValidator<T>(
    source: keyof T & string,
    target: keyof T & string,
    // TODO: find way to exclude source type
    // Omit<T, typeof source> & string not worked
  ): ValidatorFn {
    return (control: AbstractControl<T>): ValidationErrors | null => {
      const sourceCtrl = control.get(source);
      const targetCtrl = control.get(target);
      return sourceCtrl && targetCtrl && sourceCtrl.value !== targetCtrl.value
        ? { mismatch: true }
        : null;
    };
  }
}
