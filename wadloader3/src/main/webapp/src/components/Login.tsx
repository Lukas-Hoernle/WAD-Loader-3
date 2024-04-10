import { FormControl } from "@mui/material";
import Button from "@mui/material/Button";
import TextField from "@mui/material/TextField";
import { useRef } from "react";

export function Login() {
  const userRef = useRef<HTMLInputElement>(null);
  const pwRef = useRef<HTMLInputElement>(null);

  const handleClick = (username: string, pw: string) => {
    fetch("http://localhost:8080/login", {
      method: "POST",
      redirect: "follow",
      mode: "cors",
      headers: {
        "Access-Control-Allow-Origin": "*",
      },
      body: JSON.stringify({ username: username, password: pw }),
    });
  };
  return (
    <FormControl>
      <TextField inputRef={userRef} id="useranme" />
      <TextField inputRef={pwRef} id="password" type="password" />
      <Button
        onClick={() =>
          userRef.current &&
          pwRef.current &&
          handleClick(userRef.current.value, pwRef.current.value)
        }
      >
        Login
      </Button>
    </FormControl>
  );
}
