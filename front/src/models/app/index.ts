import { createEffect, createStore } from "effector";
import { Config } from "./types";

export const appInitFx = createEffect<void, Config>();

export const $criticalError = createStore<Error[]>([]);

export const $config = createStore<Config>(null as any);
