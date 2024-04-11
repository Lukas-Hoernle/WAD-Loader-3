import { Button } from "@mui/material";
import Input from "@mui/material/Input/Input";
import { useState } from "react";
import { useCookies } from "react-cookie";
import { Form, useNavigate } from "react-router-dom";

export function NewMessagePage() {
  const navigate = useNavigate();
  const [cookies] = useCookies(["XSRF-TOKEN"]);
  const [message, setMessage] = useState<{ id: number; message: string }>({id: 0, message: ""});

  const handleSubmit = () => {
    if (message) {
      fetch("/api/message", {
        method: "POST",
        headers: {
          "X-XSRF-TOKEN": cookies["XSRF-TOKEN"],
          Accept: "application/json",
          "Content-Type": "application/json",
        },
        body: JSON.stringify(message),
      }).then((res) => {
        console.debug(JSON.stringify(res));
        navigate("/");
      });
    }
  };

  return (
    <Form>
      <Input
        type="number"
        name="id"
        id="id"
        onChange={(event) =>
          setMessage((old) => {
            return {
              id: parseInt(event.target.value),
              message: old.message,
            };
          })
        }
      />
      <br />
      <Input
        type="text"
        name="message"
        id="message"
        onChange={(event) =>
          setMessage((old) => {
            return {
              id: old.id,
              message: event.target.value,
            };
          })
        }
      />
      <Button onClick={handleSubmit}>Send</Button>
    </Form>
  );
}
