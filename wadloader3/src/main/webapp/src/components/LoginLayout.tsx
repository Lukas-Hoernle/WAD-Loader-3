import { Button } from "@mui/material";
import { Outlet } from "react-router-dom";
import { login } from "../hooks/useLogin";
import { useUser } from "../hooks/useUser";

export function LoginLayout() {
  const { loading, authenticated } = useUser();

  if (loading) {
    return <p>Loading...</p>;
  }

  if (authenticated) {
    return (
      <>
        <Outlet></Outlet>
      </>
    );
  }
  return <Button onClick={login}>Login</Button>;
}
