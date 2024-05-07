import {createBrowserRouter, createRoutesFromElements, Route} from "react-router-dom";
import {LoginLayout} from "./LoginLayout.tsx";
import CreateWad from "./CreateWad";
import App from "../App.tsx";

export const router = createBrowserRouter(
    createRoutesFromElements(
        <Route
            path="/"
            element={
                <LoginLayout>
                    <App />
                    <CreateWad />
                </LoginLayout>


            }
        >
        </Route>
    )
);