import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from 'src/environments/environment';

type Credentials = {
  login: string;
  password: string;
};

@Injectable({
  providedIn: 'root',
})
export class ApiService {
  private url: string;

  constructor(private http: HttpClient) {
    this.url = environment.apiUrl;
  }

  login(body: Credentials) {
    return this.http.post(`${this.url}/auth/sign-in`, { ...body });
  }

  register(body: Credentials) {
    return this.http.post(`${this.url}/auth/register`, { ...body });
  }

  session() {
    return this.http.post(`${this.url}/session`, {}, { withCredentials: true });
  }
}
