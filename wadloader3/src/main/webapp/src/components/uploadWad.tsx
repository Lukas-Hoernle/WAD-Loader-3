import { useState } from '@mui/material';
import { Button, Box, Typography } from '@mui/material';
import CloudUploadIcon from '@mui/icons-material/CloudUpload';

const UploadWad = () => {
    const [selectedFile, setSelectedFile] = useState<File | null>(null);
    const [uploading, setUploading] = useState<boolean>(false);

    const handleFileChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const file = event.target.files?.[0];
        setSelectedFile(file || null);
    };

    const uploadFile = async (file: File) => {
        setUploading(true);
        try {
            const formData = new FormData();
            formData.append('file', file);

            const response = await fetch('/api/upload-wad', {
                method: 'POST',
                body: formData
            });

            if (response.ok) {
                alert('Datei erfolgreich hochgeladen!');
            } else {
                alert('Fehler beim Hochladen der Datei. Bitte versuchen Sie es erneut.');
            }
        } catch (error) {
            console.error('Fehler beim Hochladen der Datei:', error);
            alert('Fehler beim Hochladen der Datei. Bitte versuchen Sie es erneut.');
        } finally {
            setUploading(false);
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
            <Typography variant="h6">Datei auswählen</Typography>
            <input
                type="file"
                accept=".wad"
                style={{ display: 'none' }}
                id="file-upload"
                onChange={handleFileChange}
            />
            <label htmlFor="file-upload">
                <Button
                    variant="contained"
                    component="span"
                    startIcon={<CloudUploadIcon />}
                    disabled={uploading}
                >
                    {uploading ? 'Hochladen...' : 'Datei auswählen'}
                </Button>
            </label>
            {selectedFile && (
                <Button
                    variant="contained"
                    onClick={handleUpload}
                    disabled={uploading}
                >
                    Hochladen
                </Button>
            )}
        </Box>
    );
};

export default UploadWad;
