import { useMemo } from "react";
import { Configuration, DefaultApi } from "wadloader3-api";

export function useApi() {
  const api = useMemo(
    () =>
      new DefaultApi(
        new Configuration({
          basePath:
            window.location.port === "3000"
              ? "http://localhost:3000"
              : undefined,
        })
      ),
    []
  );

  return api;
}
