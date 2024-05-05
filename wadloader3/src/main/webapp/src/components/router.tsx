import { createBrowserRouter, createRoutesFromElements, Route } from "react-router-dom";
import { LoginLayout } from "./LoginLayout.tsx";
import App from "../App.tsx";
import HomePage from "./HomePage.tsx"; 

export const router = createBrowserRouter(
    createRoutesFromElements(
        <Route
            path="/"
            element={
                <LoginLayout>
                    <Route path="/" element={<HomePage />} />
                    <App />
                </LoginLayout>
            }
        >
        </Route>
    )
);