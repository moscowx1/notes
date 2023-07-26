import { useStore } from "effector-react";
import { $currentUser } from "../models/currentUser";
import { useNavigate } from "react-router-dom";
import { paths } from "../services/paths";
import { useEffect } from "react";

const Home = () => {
  const navigate = useNavigate();
  const currentUser = useStore($currentUser);

  useEffect(() => {
    if (!currentUser) navigate(paths.login);
  }, [currentUser]);

  return (
    <>
      <h1>Hi</h1>
    </>
  );
};

export default Home;
