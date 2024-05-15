import { useState } from 'react';
import { Button, Box, Input } from '@mui/material';
import CloudUploadIcon from '@mui/icons-material/CloudUpload';

function UploadWad() {
    const [selectedFile, setSelectedFile] = useState(null);

    const handleFileChange = (event: { target: { files: any[]; }; }) => {
        const file = event.target.files[0];
        setSelectedFile(file);
    };

    const uploadFile = async (file: any) => {
        try {
            const formData = new FormData();
            formData.append('file', file);

            await fetch('/wad/upload', {
                method: 'POST',
                body: formData
            });

            alert('Datei erfolgreich hochgeladen!');
        } catch (error) {
            console.error('Fehler beim Hochladen der Datei:', error);
            alert('Fehler beim Hochladen der Datei. Bitte versuchen Sie es erneut.');
        }
    };

    const handleUpload = () => {
        if (selectedFile) {
            uploadFile(selectedFile);
        } else {
            alert('Bitte wählen Sie eine Datei aus.');
        }
    };

    return (
        <Box>
            <Input
                type="file"
                inputProps={{ accept: '.wad' }}
                onChange={handleFileChange}
                style={{ display: 'none' }}
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

export default UploadWad;
