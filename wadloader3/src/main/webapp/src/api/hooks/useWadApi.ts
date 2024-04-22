import { useMemo } from "react";
import { WadApi } from "wadloader3-api";
import { useApiConfiguration } from "./useApiConfiguration";

export function useWadApi() {
  const config = useApiConfiguration();
  return useMemo(() => new WadApi(config), [config]);
}
