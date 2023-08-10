import { createReducer, on } from '@ngrx/store';
import { Session } from 'src/model/session';
import { AuthActions } from './auth.actions';

export type AuthState =
  | { type: 'notSet' }
  | { type: 'error'; error: Error }
  | { type: 'success'; session: Session };

export type State = {
  session: AuthState;
};

export const initialState: State = {
  session: { type: 'notSet' },
};

export const sessionReducer = createReducer(
  initialState,
  on(
    AuthActions.sessionSuccess,
    (_, { session }): State => ({ session: { type: 'success', session } }),
  ),
  on(
    AuthActions.sessionError,
    (_, { error }): State => ({ session: { type: 'error', error } }),
  ),
);
