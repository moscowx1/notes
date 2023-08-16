import { createReducer, on } from '@ngrx/store';
import { Session } from 'src/model/session';
import { AuthActions } from './auth.actions';

export type AuthState =
  | { type: 'empty' }
  | { type: 'success'; session: Session };

export type State = {
  session: AuthState;
};

export const initialState: State = {
  session: { type: 'empty' },
};

export const authReducer = createReducer(
  initialState,
  on(
    AuthActions.loggedIn,
    (_, { payload }): State => ({
      session: { type: 'success', session: payload },
    }),
  ),
  on(AuthActions.loggedOut, (): State => ({ session: { type: 'empty' } })),
);
