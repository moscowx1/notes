import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { mergeMap } from 'rxjs';
import { ApiService } from 'src/services/api.service';
import { login as loginAct } from 'src/state/session.actions';

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

    this.api
      .login({ login, password })
      .pipe(mergeMap(() => this.api.session()))
      .subscribe({
        next: (session) => {
          this.store.dispatch(loginAct(session));
          this.router.navigate(['/']);
        },
        error: () => this.loginForm.enable(),
      });
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
