import { useMemo } from "react";
import { useApiConfiguration } from "./useApiConfiguration";
import { DownloadApi } from "wadloader3-api";

export function useDownloadApi() {
  const config = useApiConfiguration();
  return useMemo(() => new DownloadApi(config), [config]);
}
