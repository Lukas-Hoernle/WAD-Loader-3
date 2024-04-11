import React from "react";
import ReactDOM from "react-dom/client";
import App from "./App.tsx";
import "./index.css";
import {
  Route,
  RouterProvider,
  createRoutesFromElements,
  createBrowserRouter,
} from "react-router-dom";
import { ShowElement } from "./components/ShowElement.tsx";
import { NewMessagePage } from "./components/NewMessagePage.tsx";
import { CookiesProvider } from "react-cookie";
import { LoginLayout } from "./components/LoginLayout.tsx";

const router = createBrowserRouter(
  createRoutesFromElements(
    <Route
      path="/"
      element={
        <LoginLayout>
          <App />
        </LoginLayout>
      }
    >
      <Route path="/entry" element={<div/>}/>
      <Route path="/show/:id" element={<ShowElement />} />
      <Route path="/new" element={<NewMessagePage />} />
    </Route>
  )
);

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <CookiesProvider>
      <RouterProvider router={router} />
    </CookiesProvider>
  </React.StrictMode>
);
