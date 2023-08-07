import { createReducer, on } from '@ngrx/store';
import { login, logout } from './session.actions';
import { Session } from 'src/model/session';

export const sessionReducer = createReducer<null | Session>(
  null,
  on(login, (_, session): Session => session),
  on(logout, (): null => null),
);
