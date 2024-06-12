import { useMemo } from "react";
import { useCookies } from "react-cookie";
import { Configuration } from "wadloader3-api";

export function useApiConfiguration() {
  // const [cookies] = useCookies(["XSRF-TOKEN"]);

  // return useMemo(
  //   () =>
  //     new Configuration({
  //       headers: {
  //         "X-XSRF-TOKEN": cookies["XSRF-TOKEN"],
  //       },
  //       basePath:
  //         // todo try to use port from configuration instead of hardcoding
  //         window.location.port === "3000" ? "http://localhost:3000" : undefined,
  //     }),
  //   []
  // );

  return new Configuration({
    // headers: {
    //   "X-XSRF-TOKEN": cookies["XSRF-TOKEN"],
    // },
    credentials: "include",

    basePath:
      // todo try to use port from configuration instead of hardcoding
      window.location.port === "3000" ? "http://localhost:3000" : undefined,
  });
}
