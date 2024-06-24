import { createBrowserRouter } from "react-router-dom";
import CreateWad from "./CreateWad";
import CreateWadPack from "./CreateWadPack";
import NavigationPage from "./NavigationPage";
import WadList from "./WadList";

export const router = createBrowserRouter([
 {
  path: "/",
  element: <NavigationPage/>,
  children: [{ path: "/", element: <HomePage /> },
  { path: "/createwad", element: <CreateWad /> },
  { path: "/createwadpack", element: <CreateWadPack /> },
  { path: "/wadlist", element: <WadList /> },],
 }
]);

function HomePage() {
  return (
    <div>
      <h1>Auswahl text</h1>
      <p>siuuuu.</p>
    </div>
  );
}
