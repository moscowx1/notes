import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { Store } from '@ngrx/store';
import { ApiService, Credentials } from 'src/services/api.service';
import { NavigatorService } from 'src/services/navigator.service';
import { AuthActions } from 'src/state/auth.actions';

type LoginForm = {
  login: FormControl<string>;
  password: FormControl<string>;
};

@Component({
  selector: 'app-login-form',
  templateUrl: './login-form.component.html',
  styleUrls: ['./login-form.component.scss'],
})
export class LoginFormComponent {
  onSubmit() {
    if (!this.loginForm.valid) return;

    this.loginForm.disable();

    const login = this.loginForm.controls['login'].value;
    const password = this.loginForm.controls['password'].value;
    const credentials: Credentials = { login, password };

    this.api.login2(credentials).subscribe({
      next: async (session) => {
        this.store.dispatch(AuthActions.loggedIn({ payload: session }));
        await this.navigator.homeRedirect();
      },
      error: () => this.loginForm.enable(),
    });
  }
  loginForm: FormGroup<LoginForm>;

  constructor(
    private api: ApiService,
    private navigator: NavigatorService,
    private store: Store,
  ) {
    const validators = [
      Validators.required,
      Validators.minLength(5),
      Validators.maxLength(30),
    ];

    this.loginForm = new FormGroup<LoginForm>({
      login: new FormControl('', { nonNullable: true, validators }),
      password: new FormControl('', { nonNullable: true, validators }),
    });
  }
}
