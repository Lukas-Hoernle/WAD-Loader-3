import { useMemo } from "react";
import { Configuration } from "wadloader3-api";

export function useApiConfiguration() {
  return useMemo(
    () =>
      new Configuration({
        basePath:
          // todo try to use port from configuration instead of hardcoding
          window.location.port === "3000" ? "http://localhost:3000" : undefined,
      }),
    []
  );
}
