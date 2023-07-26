import { useStore } from "effector-react";
import { $criticalError } from "./models/app";
import ErrorPage from "./ui/ErrorPage";
import RegisterForm from "./ui/RegisterForm";
import LoginForm from "./ui/LoginForm";
import { RouterProvider, createBrowserRouter } from "react-router-dom";
import Home from "./ui/Home";
import UnknownPage from "./ui/UnknownPage";

function App() {
  const critErrors = useStore($criticalError);
  if (critErrors.length) return <ErrorPage />;

  const router = createBrowserRouter([
    {
      path: "/",
      element: <Home />,
    },
    {
      path: "/auth",
      children: [
        {
          path: "login",
          element: <LoginForm />,
        },
        {
          path: "register",
          element: <RegisterForm />,
        },
      ],
    },
    {
      path: "*",
      element: <UnknownPage />,
    },
  ]);

  return <RouterProvider router={router} />;
}

export default App;
