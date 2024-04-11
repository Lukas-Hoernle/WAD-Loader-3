import { useEffect, useState } from "react";
import "./App.css";
import { Outlet } from "react-router-dom";

function App() {

  const [messages, setMessages] = useState<{ id: number; message: string }[]>([]);
  const [loading, setLoading] = useState(false);



  useEffect(() => {
    setLoading(true);

    fetch("/api/messages")
      .then((response) => response.json())
      .then((data) => {
        setMessages(data);
        setLoading(false);
      });
  }, []);

  if (loading) {
    return <p>Loading.1..</p>;
  }

  return (
    <div className="App">
      <header className="App-header">
        <div className="App-intro">
          <h2>JUG List</h2>
          {messages.map((message) => (
            <div key={message.id}>{message.id}: {message.message}</div>
          ))}
        </div>
      </header>
      <Outlet />
    </div>
  );
}

export default App;
