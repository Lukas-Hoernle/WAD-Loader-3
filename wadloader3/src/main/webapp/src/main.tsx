import React from "react";
import ReactDOM from "react-dom/client";

import { CookiesProvider } from "react-cookie";
import { RouterProvider } from "react-router-dom";
import { router } from "./components/router.tsx";
import "./index.css";

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <CookiesProvider>
      <RouterProvider router={router} />
    </CookiesProvider>
  </React.StrictMode>
);
