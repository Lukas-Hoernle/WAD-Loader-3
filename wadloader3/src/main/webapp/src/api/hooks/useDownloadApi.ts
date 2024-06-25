import { useMemo } from "react";
import { useApiConfiguration } from "./useApiConfiguration";
import { DownloadApi } from "wadloader3-api";
import { useCookies } from "react-cookie";

export function useDownloadApi() {
  const [cookies] = useCookies(["XSRF-TOKEN"]);
  const config = useApiConfiguration(cookies["XSRF-TOKEN"]);
  return useMemo(() => new DownloadApi(config), [config]);
}
