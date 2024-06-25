import { useCookies } from "react-cookie";

export function useLogout() {
  const [cookies] = useCookies(["XSRF-TOKEN"]);

  return () => {
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
      })
      .then(() => window.location.reload());
  };
}
