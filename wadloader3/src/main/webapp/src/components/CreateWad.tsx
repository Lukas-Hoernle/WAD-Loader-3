import { useState } from "react";
import { Button, Box, Input } from "@mui/material";
import CloudUploadIcon from "@mui/icons-material/CloudUpload";
import { useWadApi } from "../api/hooks/useWadApi";

function CreateWad() {
    const [selectedFile, setSelectedFile] = useState<Blob | undefined>();
    const [name, setName] = useState<string>("TODO-setName");
    const [description, setDescription] = useState<string>("TODO-setDescription");
    const wadApi = useWadApi();

    const handleFileChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const fileList = event.currentTarget.files;
        setName("todo-setname");
        setDescription("todo set description");
        if (!(fileList && fileList.length > 0)) {
            alert("Bitte wählen Sie eine Datei mit der Endung '.wad' aus.");
            return;
        }
        const file = fileList.item(0);
        if (file && file.name.endsWith(".wad")) {
            setSelectedFile(file);
        }
    };

    const uploadFile = async (file: Blob) => {
        await wadApi.postWad({
            name: name,
            description: description,
            file: file,
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
                inputProps={{ accept: ".wad" }}
                onChange={handleFileChange}
                style={{ display: "none" }}
                id="file-upload"
            />
            <label htmlFor="file-upload">
                <Button variant="contained" component="span" startIcon={<CloudUploadIcon />}>
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
