import { useParams } from "react-router-dom";
import { useApi } from "../hooks/useApi";
import { useEffect, useState } from "react";
import { HelloDto } from "wadloader3-api";

export function HelloPage() {
  const { name } = useParams();
  const api = useApi();

  const [greeting, setGreeting] = useState<undefined | HelloDto>();

  useEffect(() => {
    console.log("Test");
    api.helloNameGet({ name: name ?? "unkonwn" }).then(setGreeting);
  }, [api, name]);

  return <p>1{greeting && `${greeting.greeting} ${greeting.name}`}</p>;
}
