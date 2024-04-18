import { Button } from "@mui/material";
import { PropsWithChildren } from "react";
import { useLogout } from "../hooks/useLogout";
import { useUser } from "../hooks/useUser";
import { login } from "../hooks/useLogin";



export function LoginLayout({ children }: PropsWithChildren) {

  const logout = useLogout();
  const {loading, authenticated } = useUser();

  if (loading) {
    return <p>Loading...</p>;
  }

  if (authenticated) {
    return (
      <>
        <Button onClick={logout}>Logout</Button>
        {children}
      </>
    );
  }
  return <Button onClick={login}>Login</Button>;
}
