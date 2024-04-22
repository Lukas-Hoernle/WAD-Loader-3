import { useMemo } from "react";
import { useApiConfiguration } from "./useApiConfiguration";
import { WadpackApi } from "wadloader3-api";

export function useWadPackApi() {
  const config = useApiConfiguration();
  return useMemo(() => new WadpackApi(config), [config]);
}
