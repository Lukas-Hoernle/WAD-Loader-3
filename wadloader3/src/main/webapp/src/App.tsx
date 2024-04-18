import "./App.css";
import { Outlet } from "react-router-dom";
import { useUser } from "./hooks/useUser";

function App() {
  const { user, loading } = useUser();

  return (
    <>
      <Outlet />
      <p>Hello {loading ? "unknown dude" : user?.name}</p>
    </>
  );
}

export default App;
