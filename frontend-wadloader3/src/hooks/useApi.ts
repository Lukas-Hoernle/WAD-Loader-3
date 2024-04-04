import { useMemo } from "react";
import { DefaultApi } from "wadloader3-api";

export function useApi() {
    const api = useMemo(() => new DefaultApi,[]);
    return api;
}