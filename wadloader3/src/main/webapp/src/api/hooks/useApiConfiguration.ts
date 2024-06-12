import { useMemo } from "react";
import { Configuration } from "wadloader3-api";

export function useApiConfiguration(xsrfToken: string) {
  console.error(xsrfToken);
  return useMemo(() => new Configuration({
    headers: {
      "X-XSRF-TOKEN": xsrfToken,
    },
    basePath:
      window.location.port === "3000" ? "http://localhost:3000" : undefined,
  }),[xsrfToken]);
}
