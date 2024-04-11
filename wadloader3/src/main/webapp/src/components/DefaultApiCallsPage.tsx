import { useEffect, useState } from "react";
import { useCookies } from "react-cookie";
import { HelloDto } from "wadloader3-api";
import { useApi } from "../hooks/useApi";

export function DefaultApiCallsPage() {
  const [cookies] = useCookies(["XSRF-TOKEN"]);
  const [greeting, setGreeting] = useState<HelloDto>();

  const api = useApi();

  useEffect(() => {
    api
      .apiHelloNameGet({ name: "my-test-name" })
      .then((res) => setGreeting(res));
  }, [cookies]);

  return greeting ? (
    <p>
      {greeting.greeting} {greeting.name}
    </p>
  ) : (
    <p>Idk you. Fuck off</p>
  );
}
