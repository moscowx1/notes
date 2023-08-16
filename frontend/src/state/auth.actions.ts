import { createActionGroup, emptyProps, props } from '@ngrx/store';
import { Session } from 'src/model/session';

export const AuthActions = createActionGroup({
  source: 'Auth API',
  events: {
    loggedIn: props<{ payload: Session }>(),
    loggedOut: emptyProps(),
  },
});
