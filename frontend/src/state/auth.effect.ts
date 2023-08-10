import { inject } from '@angular/core';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { catchError, exhaustMap, map, mergeMap, of } from 'rxjs';
import { ApiService } from 'src/services/api.service';
import { AuthActions } from './auth.actions';

export const login = createEffect(
  (actions$ = inject(Actions), apiService = inject(ApiService)) => {
    return actions$.pipe(
      ofType(AuthActions.login),
      exhaustMap(({ credentials }) =>
        apiService.login(credentials).pipe(
          mergeMap(() =>
            apiService.session().pipe(
              map((session) => AuthActions.sessionSuccess({ session })),
              catchError((e) => of(AuthActions.sessionError(e))),
            ),
          ),
        ),
      ),
    );
  },
  { functional: true },
);

export const register = createEffect(
  (actions$ = inject(Actions), apiService = inject(ApiService)) => {
    return actions$.pipe(
      ofType(AuthActions.register),
      exhaustMap(({ credentials }) =>
        apiService.register(credentials).pipe(
          mergeMap(() =>
            apiService.session().pipe(
              map((session) => AuthActions.sessionSuccess({ session })),
              catchError((e) => of(AuthActions.sessionError(e))),
            ),
          ),
        ),
      ),
    );
  },
  { functional: true },
);
