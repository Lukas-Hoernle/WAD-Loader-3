import { useState } from "react";
import { Button, Box, Input } from "@mui/material";
import { useWadApi } from "../api/hooks/useWadApi";
import { useCookies } from "react-cookie";

function CreateWad() {
    const [selectedFile, setSelectedFile] = useState<Blob | undefined>();
    const [name, setName] = useState<string>("TODO-setName");
    const [description, setDescription] = useState<string>("TODO-setDescription");
    const wadApi = useWadApi();

    const handleFileChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const fileList = event.currentTarget.files;
        
        setDescription("todo set description");
        if (!(fileList && fileList.length > 0)) {
            alert("Bitte wählen Sie eine Datei mit der Endung '.wad' aus.");
            return;
        }
        const file = fileList.item(0);
        if (file && file.name.endsWith(".pk3")) {
            setName(file.name);
            setSelectedFile(file);
        }
        console.log(wadApi.getWads().then(JSON.stringify).then(console.log))
    };

    const [cookies] = useCookies(["XSRF-TOKEN"]);
    const uploadFile = async (file: Blob) => {
        await wadApi.postWad({
            name: name,
            description: description,
            file: file,
        }, {
            headers: {
                "X-XSRF-TOKEN": cookies["XSRF-TOKEN"]
            }
        });
    };

    const handleUpload = () => {
        if (selectedFile) {
            uploadFile(selectedFile);
        } else {
            alert("Bitte wählen Sie eine Datei aus.");
        }
    };

    return (
        <Box>
            <Input
                type="file"
                inputProps={{ accept: ".pk3" }}
                onChange={handleFileChange}
                style={{ display: "none" }}
                id="file-upload"
            />
            <label htmlFor="file-upload">
                <Button variant="contained" component="span">
                    Datei auswählen
                </Button>
            </label>
            {selectedFile && (
                <Button variant="contained" onClick={handleUpload}>
                    Hochladen
                </Button>
            )}
        </Box>
    );
}

export default CreateWad;
