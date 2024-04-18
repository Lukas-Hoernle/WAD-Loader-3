import React from "react";
import ReactDOM from "react-dom/client";

import "./index.css";
import {
  RouterProvider,
} from "react-router-dom";
import { CookiesProvider } from "react-cookie";
import { router } from "./components/router.tsx";


ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <CookiesProvider>
      <RouterProvider router={router} />
    </CookiesProvider>
  </React.StrictMode>
);
