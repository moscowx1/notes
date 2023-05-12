interface LoginData {
  login: string;
  password: string;
}

class Api {
  constructor(
    private baseUrl: string) {}

  async login(data: LoginData) {
    const response = await fetch(`${this.baseUrl}/auth/sign-in`, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-type': 'application/json',
        Accept: 'application/json',
      },
      credentials: 'same-origin'
    })
  }
}