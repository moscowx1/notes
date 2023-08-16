import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ApiService } from 'src/services/api.service';
import { CustomValidators } from 'src/utils/CustomValidators';
import { Store } from '@ngrx/store';
import { AuthActions } from 'src/state/auth.actions';
import { NavigatorService } from 'src/services/navigator.service';

type RegisterForm = {
  login: FormControl<string>;
  password: FormControl<string>;
  repassword: FormControl<string>;
};

@Component({
  selector: 'app-register-form',
  templateUrl: './register-form.component.html',
  styleUrls: ['./register-form.component.scss'],
})
export class RegisterFormComponent {
  registerForm: FormGroup<RegisterForm>;

  onSubmit() {
    if (!this.registerForm.valid) return;

    this.registerForm.disable();

    const login = this.registerForm.controls['login'].value;
    const password = this.registerForm.controls['password'].value;

    this.apiService.register2({ login, password }).subscribe({
      next: async (session) => {
        this.store.dispatch(AuthActions.loggedIn({ payload: session }));
        await this.navigator.homeRedirect();
      },
      error: () => this.registerForm.enable(),
    });
  }

  constructor(
    private apiService: ApiService,
    private navigator: NavigatorService,
    private store: Store,
  ) {
    const validators = [
      Validators.required,
      Validators.minLength(5),
      Validators.maxLength(30),
    ];

    this.registerForm = new FormGroup<RegisterForm>(
      {
        login: new FormControl('', { nonNullable: true, validators }),
        password: new FormControl('', { nonNullable: true, validators }),
        repassword: new FormControl('', { nonNullable: true, validators }),
      },
      {
        validators: CustomValidators.MatchValidator<RegisterForm>(
          'password',
          'repassword',
        ),
      },
    );
  }
}
