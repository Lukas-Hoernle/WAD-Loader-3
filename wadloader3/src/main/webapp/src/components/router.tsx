import {createBrowserRouter, createRoutesFromElements, Route} from "react-router-dom";
import {LoginLayout} from "./LoginLayout.tsx";
import App from "../App.tsx";
import UploadWad from "./UploadWad.tsx";

export const router = createBrowserRouter(
    createRoutesFromElements(
        <Route
            path="/"
            element={
                <LoginLayout>
                    <Route path="/upload" element={<UploadWad />} />
                    <App />
                </LoginLayout>
            }
        >
        </Route>
    )
);