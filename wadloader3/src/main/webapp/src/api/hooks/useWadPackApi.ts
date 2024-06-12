import { useMemo } from "react";
import { useApiConfiguration } from "./useApiConfiguration";
import { WadpackApi } from "wadloader3-api";
import { useCookies } from "react-cookie";

export function useWadPackApi() {
  const [cookies] = useCookies(["XSRF-TOKEN"]);
  const config = useApiConfiguration(cookies["XSRF-TOKEN"] );
  return useMemo(() => new WadpackApi(config), [config]);
}
