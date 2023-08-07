import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { mergeMap } from 'rxjs';
import { ApiService } from 'src/services/api.service';
import { CustomValidators } from 'src/utils/CustomValidators';
import { login as loginAct } from 'src/state/session.actions';
import { Store } from '@ngrx/store';

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

    this.apiService
      .register({ login, password })
      .pipe(mergeMap(() => this.apiService.session()))
      .subscribe({
        next: (session) => {
          this.store.dispatch(loginAct(session));
          this.router.navigate(['/']);
        },
        error: () => this.registerForm.enable(),
      });
  }

  constructor(
    private apiService: ApiService,
    private router: Router,
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
