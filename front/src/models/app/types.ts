import zod from "zod";

const _appMode = ["Debug", "Production"] as const;
export const appMode = zod.enum(_appMode);
export type AppMode = zod.infer<typeof appMode>;

const _scheme = ["http", "https"] as const;
export const scheme = zod.enum(_scheme);
export type Scheme = zod.infer<typeof scheme>;

export const config = zod.object({
  baseUrl: zod.string().url(),
  appMode,
  scheme,
});
export type Config = zod.infer<typeof config>;
