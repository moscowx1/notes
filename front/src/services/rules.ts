import { Rule } from "effector-forms";

export const rules = {
  minLength: (min: number): Rule<string> => ({
    name: "minLength",
    validator: (value) => value.length >= min,
  }),
};
