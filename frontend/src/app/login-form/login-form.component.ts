import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ApiService } from 'src/services/api.service';

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

    const login = this.loginForm.controls['login'].value;
    const password = this.loginForm.controls['password'].value;

    this.api.login({ login, password }).subscribe({
      next: () => {
        this.api.session().subscribe((d) => {
          console.log(d);
        });
      },
    });
  }
  loginForm: FormGroup<LoginForm>;

  constructor(private api: ApiService) {
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
