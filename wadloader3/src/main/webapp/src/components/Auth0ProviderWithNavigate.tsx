import { AppState, Auth0Provider, User } from "@auth0/auth0-react";
import { Outlet, useNavigate } from "react-router-dom";

export const Auth0ProviderWithNavigate = () => {
  const navigate = useNavigate();

  //   const domain = process.env.REACT_APP_AUTH0_DOMAIN;
  //   const clientId = process.env.REACT_APP_AUTH0_CLIENT_ID;
  //   const redirectUri = process.env.REACT_APP_AUTH0_CALLBACK_URL;
  //   const audience = process.env.REACT_APP_AUTH0_AUDIENCE;

  const domain = "dev-5d1wp1lbqdurw3gf.eu.auth0.com";
  const clientId = "9uLl1oryQK4cASNfbJyFIfzQXOzUyxiS";
  //   use localhost:8080 if serving from Spring
  //   const redirectUri = "http:localhost:3000/login/oauth2/code/okta";
  const redirectUri = "http:localhost:3000/hello";
  const audience = "https://wadloader3.api";

  const onRedirectCallback: (appState?: AppState, user?: User) => void = (
    appState
  ) => {
    navigate(appState?.returnTo || window.location.pathname);
  };

  if (!(domain && clientId && redirectUri && audience)) {
    return null;
  }

  return (
    <Auth0Provider
      domain={domain}
      clientId={clientId}
      authorizationParams={{
        audience: audience,
        redirect_uri: redirectUri,
      }}
      onRedirectCallback={onRedirectCallback}
    >
      <p>Hallo</p>
      <Outlet />
      <p>Tschüss</p>
    </Auth0Provider>
  );
};
