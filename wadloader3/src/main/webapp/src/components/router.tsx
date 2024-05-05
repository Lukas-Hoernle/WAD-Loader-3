import {createBrowserRouter, createRoutesFromElements, Route} from "react-router-dom";
import {LoginLayout} from "./LoginLayout.tsx";
import App from "../App.tsx";

export const router = createBrowserRouter(
    createRoutesFromElements(
        <Route
            path="/"
            element={
                <LoginLayout>
                    <Route path="/" element={<HomePage />} />
                    <Route path="/search-download" element={<SearchDownloadPage />} />
                    <Route path="/create-wadpack" element={<CreateWadPackPage />} />
                    <App />
                </LoginLayout>
            }
        >
        </Route>
    )
);
