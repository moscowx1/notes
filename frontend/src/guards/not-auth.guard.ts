import { inject } from '@angular/core';
import { CanActivateFn } from '@angular/router';
import { Store } from '@ngrx/store';
import { map } from 'rxjs';
import { State, selectIsAuthentificated } from 'src/state/auth.reducer';

export const notAuthGuard: CanActivateFn = () => {
  const store = inject<Store<State>>(Store);
  return store.select(selectIsAuthentificated).pipe(map((isAuth) => !isAuth));
};
