import { Box, Button, Container, Typography } from "@mui/material";
import { useStore } from "effector-react";
import { $config, $criticalError } from "../models/app";

const ErrorPage = () => {
  const critErrors = useStore($criticalError);
  const { appMode } = useStore($config) || {};
  if (appMode === "Debug")
    return (
      <ul>
        {critErrors.map((e) => {
          return (
            <li>
              `${e.name} {e.message}`
            </li>
          );
        })}
      </ul>
    );

  return (
    <Container>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          alignItems: "center",
          justifyContent: "center",
        }}
      >
        <Typography component="h1" variant="h5">
          Something went wrong
        </Typography>
        <Button onClick={() => window.location.reload()}>
          Try to reload page
        </Button>
      </Box>
    </Container>
  );
};

export default ErrorPage;
