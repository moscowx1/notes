import { createActionGroup, props } from '@ngrx/store';
import { Session } from 'src/model/session';
import { Credentials } from 'src/services/api.service';

export const AuthActions = createActionGroup({
  source: 'Session API',
  events: {
    login: props<{ credentials: Credentials }>(),
    register: props<{ credentials: Credentials }>(),
    sessionSuccess: props<{ session: Session }>(),
    sessionError: props<{ error: Error }>(),
  },
});

