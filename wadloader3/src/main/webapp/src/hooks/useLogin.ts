export function useLogin() {
    return login;
}

export function login() {
  let port = window.location.port ? ":" + window.location.port : "";
  if (port === ":3000") {
    port = ":8080";
  }
  // redirect to a protected URL to trigger authentication
  window.location.href = `//${window.location.hostname}${port}/api`;
}
