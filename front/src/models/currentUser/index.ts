import { createEffect, createEvent, createStore } from "effector";
import { Credential, RegisterData } from "./types";

export const $currentUser = createStore<string | null>(null);
export const $currentUserError = createStore<Error | null>(null);

export const logout = createEvent();

export const loginFx = createEffect<Credential, void>();
export const registerFx = createEffect<RegisterData, void>();
