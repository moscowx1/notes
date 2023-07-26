import { Link } from "@mui/material";
import { paths } from "../services/paths";

const UnknownPage = () => {
  return (
    <>
      <h2>Page not exists</h2>
      <Link href={paths.home}>Go home</Link>
    </>
  );
};

export default UnknownPage;
