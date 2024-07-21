import { createBrowserRouter } from "react-router-dom";
import CreateWad from "./CreateWad";
import CreateWadPack from "./CreateWadPack";
import { LoginLayout } from "./LoginLayout";
import NavigationPage from "./NavigationPage";
import WadList from "./WadList";
import { HomePage } from "./HomePage";

export const router = createBrowserRouter([
  {
    path: "/",
    element: <LoginLayout />,
    children: [
      {
        path: "/",
        element: <NavigationPage />,
        children: [
          { path: "/", element: <HomePage /> },
          { path: "/createwad", element: <CreateWad /> },
          { path: "/createwadpack", element: <CreateWadPack /> },
          { path: "/listwad", element: <WadList /> },
        ],
      },
    ],
  },
]);

