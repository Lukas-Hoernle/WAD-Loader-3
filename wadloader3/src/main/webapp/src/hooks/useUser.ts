import { useEffect, useState } from "react";

export function useUser() {
  const [authenticated, setAuthenticated] = useState(false);
  const [loading, setLoading] = useState(false);
  //user is a whole oauth2 principal
  const [user, setUser] = useState<{ name: string } | undefined>(undefined);

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

  return {
    authenticated,
    loading,
    user
  };
}
