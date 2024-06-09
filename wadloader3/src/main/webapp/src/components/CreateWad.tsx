import { useState } from "react";
import { Button, Box, Input } from "@mui/material";
import CloudUploadIcon from "@mui/icons-material/CloudUpload";
import { useWadPackApi } from "../api/hooks/useWadPackApi";

function CreateWad() {
    const [selectedFile, setSelectedFile] = useState(null);
    const wadPackApi = useWadPackApi();

    const handleFileChange = (event: { target: { files: any[]; }; }) => {
        const file = event.target.files[0];
        if (file && file.name.endsWith(".wad")) {
            setSelectedFile(file);
        } else {
            alert("Bitte wählen Sie eine Datei mit der Endung '.wad' aus.");
        }
    };

    const uploadFile = async (file: string | Blob) => {
        try {
            const formData = new FormData();
            formData.append("file", file);

            await wadPackApi.postWadpack(formData);

            alert("Datei erfolgreich hochgeladen!");
        } catch (error) {
            console.error("Fehler beim Hochladen der Datei:", error);
            alert("Fehler beim Hochladen der Datei. Bitte versuchen Sie es erneut.");
        }
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
