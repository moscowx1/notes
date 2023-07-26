export type Credential = {
  login: string;
  password: string;
};

export type RegisterData = Credential & {
  rePassword: string;
};
