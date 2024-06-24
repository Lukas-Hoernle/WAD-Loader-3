import { Button } from "@mui/material";
import { Outlet } from "react-router-dom";
import { login } from "../hooks/useLogin";
import { useLogout } from "../hooks/useLogout";
import { useUser } from "../hooks/useUser";

export function LoginLayout() {
  const logout = useLogout();
  const { loading, authenticated } = useUser();

  if (loading) {
    return <p>Loading...</p>;
  }

  if (authenticated) {
    return (
      <>
        <Button onClick={logout}>Logout</Button>
        <Outlet></Outlet>
      </>
    );
  }
  return <Button onClick={login}>Login</Button>;
}
