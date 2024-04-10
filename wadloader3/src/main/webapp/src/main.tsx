import React from "react";
import ReactDOM from "react-dom/client";
import App from "./App.tsx";
import "./index.css";
import {
    Route,
    RouterProvider,
    createRoutesFromElements, createBrowserRouter,
} from "react-router-dom";
import { ShowElement } from "./components/ShowElement.tsx";

const router = createBrowserRouter(
  createRoutesFromElements(
    <Route path="/" element={<App/>}>
      <Route path="/show/:id" element={<ShowElement />}/>
    </Route>
  )
);

ReactDOM.createRoot(document.getElementById("root")!).render(
    <React.StrictMode>
      <RouterProvider router={router} />
    </React.StrictMode>
);
