import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { LoginFormComponent } from './login-form/login-form.component';
import { RegisterFormComponent } from './register-form/register-form.component';
import { MainComponent } from './main/main.component';
import { notAuthGuard } from 'src/guards/not-auth.guard';

const routes: Routes = [
  { path: 'login', component: LoginFormComponent, canActivate: [notAuthGuard] },
  { path: 'register', component: RegisterFormComponent, canActivate: [notAuthGuard]},
  { path: '**', component: MainComponent },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule],
})
export class AppRoutingModule {}
