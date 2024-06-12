import { useMemo } from "react";
import { WadApi } from "wadloader3-api";
import { useApiConfiguration } from "./useApiConfiguration";
import { useCookies } from "react-cookie";

export function useWadApi() {
  const [cookies] = useCookies(["XSRF-TOKEN"]);
  const config = useApiConfiguration(cookies["XSRF-TOKEN"] );
  return useMemo(() => new WadApi(config), [config]);
}
