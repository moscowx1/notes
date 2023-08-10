import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { mergeMap } from 'rxjs';
import { ApiService, Credentials } from 'src/services/api.service';
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

    this.api.login2(credentials).subscribe(

    )

    this.store.dispatch(AuthActions.login({ credentials }));
  }
  loginForm: FormGroup<LoginForm>;

  constructor(
    private api: ApiService,
    private router: Router,
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
