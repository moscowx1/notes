import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { map } from 'rxjs';
import { environment } from 'src/environments/environment';
import { session } from 'src/model/session';

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
    return this.http.post(
      `${this.url}/auth/sign-in`,
      { ...body },
      { withCredentials: true },
    );
  }

  register(body: Credentials) {
    return this.http.post(
      `${this.url}/auth/register`,
      { ...body },
      { withCredentials: true },
    );
  }

  session() {
    return this.http
      .post(`${this.url}/session`, {}, { withCredentials: true })
      .pipe(map((json) => session.parse(json)));
  }
}
