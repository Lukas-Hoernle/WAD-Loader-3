import { Button } from "@mui/material";
import { PropsWithChildren, useEffect, useState } from "react";
import { useCookies } from "react-cookie";



export function LoginLayout({ children }: PropsWithChildren) {
  const [authenticated, setAuthenticated] = useState(false);
  const [loading, setLoading] = useState(false);
  //user is a whole oauth2 principal
  const [user, setUser] = useState<{name: string} |undefined>(undefined);
  const [cookies] = useCookies(["XSRF-TOKEN"]);

  useEffect(() => {
    setLoading(true);
    fetch("/api/user", { credentials: "include" })
      .then((response) => response.text())
      .then((body) => {
        if (body === "") {
          setAuthenticated(false);
        } else {
          setUser(JSON.parse(body));
          setAuthenticated(true);
        }
        setLoading(false);
      });
  }, [setAuthenticated, setLoading, setUser]);

  const login = () => {
    let port = window.location.port ? ":" + window.location.port : "";
    if (port === ":3000") {
      port = ":8080";
    }
    // redirect to a protected URL to trigger authentication
    window.location.href = `//${window.location.hostname}${port}/api/private`;
  };

  if (loading) {
    return <p>Loading...</p>;
  }

  const logout = () => {
    fetch("/api/logout", {
      method: "POST",
      credentials: "include",
      headers: { "X-XSRF-TOKEN": cookies["XSRF-TOKEN"] },
    })
      .then((res) => res.json())
      .then((response) => {
        window.location.href =
          `${response.logoutUrl}?id_token_hint=${response.idToken}` +
          `&post_logout_redirect_uri=${window.location.origin}`;
      });
  };

  if (authenticated) {
    return (
      <>
        <Button onClick={logout}>Logout</Button>
        <br />
        <p>Hello {user && user.name}</p>
        <br />
        {children}
      </>
    );
  }
  return <Button onClick={login}>Login</Button>;
}
