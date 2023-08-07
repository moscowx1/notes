import { z } from 'zod';

const role = z.enum(['UserRole', 'AdminRole']);
export type Role = z.infer<typeof role>;

export const session = z.object({
  login: z.string().min(5),
  role: role,
});

export type Session = z.infer<typeof session>;
