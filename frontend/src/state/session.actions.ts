import { createAction, props } from '@ngrx/store';
import { Session } from 'src/model/session';

export const login = createAction(
  '[Login Register Component] Login',
  props<Session>(),
);
export const logout = createAction('[Menu] Logout');
