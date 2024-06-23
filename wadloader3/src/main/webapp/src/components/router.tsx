import { createBrowserRouter, createRoutesFromElements, Route } from "react-router-dom";
import { MainLayout } from "./MainLayout.tsx";
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
                    <MainLayout>
                        <WadList />
                    </MainLayout>
                }
            />
            <Route
                path="/create-wad"
                element={
                    <MainLayout>
                        <CreateWad />
                    </MainLayout>
                }
            />
            <Route
                path="/create-wad-pack"
                element={
                    <MainLayout>
                        <CreateWadPack />
                    </MainLayout>
                }
            />
        </>
    )
);
