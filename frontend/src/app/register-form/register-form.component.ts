import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ApiService } from 'src/services/api.service';
import { CustomValidators } from 'src/utils/CustomValidators';

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
  onSubmit() {
    if (!this.registerForm.valid) return;

    const login = this.registerForm.controls['login'].value;
    const password = this.registerForm.controls['password'].value;
    this.apiService.register({ login, password }).subscribe({
      next: (data) => {
        console.log(data);
      },
      error: (e) => {
        console.log(e);
      },
      complete: () => {
        console.log(1);
      },
    });
  }
  registerForm: FormGroup<RegisterForm>;
  constructor(private apiService: ApiService) {
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
