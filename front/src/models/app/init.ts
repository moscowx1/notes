import { $config, $criticalError, appInitFx } from ".";
import { config } from "./types";

appInitFx.use(() => {
  const values = process.env;
  return config.parse({
    baseUrl: values.REACT_APP_BASE_URL,
    appMode: values.REACT_APP_MODE,
    scheme: values.REACT_APP_SCHEME,
  });
});

$config.on(appInitFx.doneData, (_, config) => config);

$criticalError.on(appInitFx.failData, (st, e) => {
  console.error(e);
  return [...st, e];
});

appInitFx();
