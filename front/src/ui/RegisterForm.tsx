import {
  Alert,
  Box,
  Button,
  Container,
  CssBaseline,
  Link,
  TextField,
  Typography,
} from "@mui/material";
import React from "react";
import { $currentUserError, registerFx } from "../models/currentUser";
import { createForm, useForm } from "effector-forms";
import { rules } from "../services/rules";
import { useStore } from "effector-react";
import { forward } from "effector";
import { paths } from "../services/paths";

const registerForm = createForm({
  fields: {
    login: {
      init: "",
      rules: [rules.minLength(5)],
    },
    password: {
      init: "",
      rules: [rules.minLength(5)],
    },
    rePassword: {
      init: "",
      rules: [
        {
          name: "passwordMatch",
          validator: (rePassword, { password }) => rePassword === password,
        },
      ],
    },
  },
  validateOn: ["change", "submit", "blur"],
});

forward({
  from: registerForm.formValidated,
  to: registerFx,
});

const RegisterForm = () => {
  const { fields, submit, eachValid, hasError } = useForm(registerForm);
  const pending = useStore(registerFx.pending);
  const error = useStore($currentUserError);

  const onSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    submit();
  };

  return (
    <Container component="main" maxWidth="xs">
      <CssBaseline />
      <Box
        sx={{
          marginTop: 8,
          display: "flex",
          flexDirection: "column",
          alignItems: "center",
        }}
      >
        <Typography component="h1" variant="h5">
          Register
        </Typography>
        <Box component="form" noValidate sx={{ mt: 1 }} onSubmit={onSubmit}>
          <TextField
            margin="normal"
            fullWidth
            label="Login"
            autoFocus
            disabled={pending}
            value={fields.login.value}
            error={hasError("login")}
            onChange={(e) => fields.login.onChange(e.target.value)}
          />
          <TextField
            margin="normal"
            fullWidth
            label="Password"
            type="password"
            disabled={pending}
            error={hasError("password")}
            value={fields.password.value}
            onChange={(e) => fields.password.onChange(e.target.value)}
          />
          <TextField
            margin="normal"
            fullWidth
            label="Reenter Password"
            type="password"
            disabled={pending}
            error={hasError("rePassword")}
            value={fields.rePassword.value}
            onChange={(e) => fields.rePassword.onChange(e.target.value)}
          />
          <Button
            type="submit"
            fullWidth
            variant="contained"
            disabled={pending || !eachValid}
            sx={{ mt: 3, mb: 2 }}
          >
            Sign Up
          </Button>
          {!!error && <Alert severity="error">error register</Alert>}
          <Link href={paths.login} variant="body2">
            Already have account? Login
          </Link>
        </Box>
      </Box>
    </Container>
  );
};

export default RegisterForm;
