import { useEffect, useState } from "react";
import { useCookies } from "react-cookie";
import { useParams } from "react-router-dom";

export function ShowElement() {
  const { id } = useParams();
  const [cookies] = useCookies(['XSRF-TOKEN']);
  const [message, setMessage] = useState<{id: number, message: string}>();

  useEffect(() => {
    fetch(`/api/message/${id}`, {
        headers: {
            'X-XSRF-TOKEN': cookies['XSRF-TOKEN']
        }
    })
      .then((response) => response.json())
      .then((data) => setMessage(data));
  }, [cookies, id]);

  return (
    <>
      <p>EPIC MESSAGE:</p>
      <p>{message?.id}: {message?.message}</p>
    </>
  );
}
