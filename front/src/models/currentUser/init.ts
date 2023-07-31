import {
  $currentUser,
  $currentUserError,
  loginFx,
  logout,
  registerFx,
} from ".";
import { requestBase } from "../../services/request";

loginFx.use(async (data) => {
  return requestBase(
    "auth/sign-in",
    "POST",
  )(data).then((r) => {
    if (!r.ok) throw new Error("not success status code");
    return r.json();
  });
});

registerFx.use(async (data) => {
  return requestBase(
    "auth/register",
    "POST",
  )(data).then((r) => {
    if (!r.ok) throw new Error("not success status code");
    return r.json();
  });
});

$currentUserError
  .reset(logout)
  .on([loginFx.failData, registerFx.failData], (_, e) => {
    console.error(e);
    return e;
  });

$currentUser.reset(logout).on(loginFx.doneData, (_, u) => u);
