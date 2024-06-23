import { createBrowserRouter, createRoutesFromElements, Route } from "react-router-dom";
import { LoginLayout } from "./LoginLayout.tsx";
import CreateWad from "./CreateWad";
import CreateWadPack from "./CreateWadPack";
import WadList from "./WadList";
import App from "../App.tsx";

export const router = createBrowserRouter(
    createRoutesFromElements(
        <>
            <Route
                path="/"
                element={
                    <LoginLayout>
                        <App />
                        <CreateWad />
                    </LoginLayout>
                }
            />
            <Route
                path="/create-wad-pack"
                element={<CreateWadPack />}
            />
            <Route
                path="/abc"
                element={<p>adsad</p>}
            />
            <Route
                path="/wadliste"
                element={<WadList />}
            />
        </>
    )
);
