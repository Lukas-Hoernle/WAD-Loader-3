import { useState } from "react";
import { Button, Box, Input } from "@mui/material";
import CloudUploadIcon from "@mui/icons-material/CloudUpload";

function CreateWad() {
    const [selectedFile, setSelectedFile] = useState(null);

    const handleFileChange = (event) => {
        const file = event.target.files[0];
        if (file && file.name.endsWith(".wad")) {
            setSelectedFile(file);
        } else {
            // Bruder was lädst du hoch?
            alert("Bitte wählen Sie eine Datei mit der Endung '.wad' aus.");
        }
    };

    const handleUpload = () => {
        // upload api fun
        console.log("Upload der Datei:", selectedFile);
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
