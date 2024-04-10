import React from "react";
import ReactDOM from "react-dom/client";
import App from "./App.tsx";
import "./index.css";
import {
  Route,
  RouterProvider,
  createHashRouter,
  createRoutesFromElements,
} from "react-router-dom";
import { HelloPage } from "./components/HelloPage.tsx";

import { Auth0ProviderWithNavigate } from "./components/Auth0ProviderWithNavigate.tsx";

const router = createHashRouter(
  createRoutesFromElements(
    <Route path="/" element={<Auth0ProviderWithNavigate />}>
      <Route path="hello/" element={<App />}>
        <Route path=":name" element={<HelloPage />} />
      </Route>
    </Route>
  )
);

ReactDOM.createRoot(document.getElementById("root")!).render(
    <React.StrictMode>
      <RouterProvider router={router} />
    </React.StrictMode>
);
