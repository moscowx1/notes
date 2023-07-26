import { $config } from "../models/app";

export type Method = "GET" | "POST";

export const requestBase =
  (path: string, method: Method) =>
  async <TBody>(body: TBody): Promise<Response> => {
    const { baseUrl, scheme } = $config.getState();
    return await fetch(`${scheme}://${baseUrl}/${path}`, {
      method,
      headers: {
        Accept: "application/json",
        "Content-type": "application/json",
      },
      body: JSON.stringify(body),
    });
  };
