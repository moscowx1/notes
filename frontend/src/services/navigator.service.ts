import { Injectable } from '@angular/core';
import { Router } from '@angular/router';

@Injectable({
  providedIn: 'root',
})
export class NavigatorService {
  constructor(private router: Router) {}

  login() {
    return '/login';
  }

  loginRedirect() {
    return this.router.navigateByUrl(this.login());
  }

  home() {
    return '';
  }

  homeRedirect() {
    return this.router.navigateByUrl(this.home());
  }

  register() {
    return 'register';
  }

  registerRedirect() {
    return this.router.navigateByUrl(this.register());
  }
}
