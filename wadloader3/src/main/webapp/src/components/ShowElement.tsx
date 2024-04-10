import { useEffect, useState } from "react";
import { useParams } from "react-router-dom";

export function ShowElement() {
  const { id } = useParams();

  const [message, setMessage] = useState<{id: number, message: string}>();

  useEffect(() => {
    fetch(`/api/message/${id}`)
      .then((response) => response.json())
      .then((data) => setMessage(data));
  }, [id]);

  return (
    <>
      <p>EPIC MESSAGE:</p>
      <p>{message?.id}: {message?.message}</p>
    </>
  );
}
