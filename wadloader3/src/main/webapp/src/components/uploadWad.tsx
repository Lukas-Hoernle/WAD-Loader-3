import React, { useState } from 'react';
import { Button, Typography, Paper, Grid, LinearProgress } from '@mui/material';
import CloudUploadIcon from '@mui/icons-material/CloudUpload';
import { WadApi } from 'wadloader3-api';
import { useWadApi } from '../api/hooks/useWadApi';

const UploadWad = () => {
    const [selectedFile, setSelectedFile] = useState<File | null>(null);
    const [uploading, setUploading] = useState<boolean>(false);
    const wadApi = useWadApi();
    const handleFileChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const file = event.target.files?.[0];
        setSelectedFile(file || null);
    };

    const handleUpload = () => {
        if (selectedFile) {
            uploadFile(selectedFile);
        } else {
            alert('Bitte wählen Sie eine Datei aus.');
        }
    };

    const uploadFile = async (file: File) => {
        setUploading(true);
        try {
            const formData = new FormData();
            formData.append('file', file);

            wadApi.postWad({name: "habicht", description:"habichtdesc",file: file})
            .then(_=>{setSelectedFile(null)})
            .catch(()=>console.log("shit hits the fan"))
            
        } catch (error) {
            console.error('Fehler beim Hochladen der Datei:', error);
            alert('Fehler beim Hochladen der Datei. Bitte versuchen Sie es erneut.');
        } finally {
            setUploading(false);
        }
    };

    return (
        <Paper elevation={3} sx={{ p: 3, mt: 3, mx: 'auto', maxWidth: 600 }}>
            <Typography variant="h4" align="center" gutterBottom>
                Datei hochladen
            </Typography>
            <Grid container spacing={2} alignItems="center">
                <Grid item xs={12}>
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
                            fullWidth
                            disabled={uploading}
                        >
                            {selectedFile ? selectedFile.name : 'Datei auswählen'}
                        </Button>
                    </label>
                </Grid>
                <Grid item xs={12}>
                    {selectedFile && (
                        <Button
                            variant="contained"
                            color="primary"
                            onClick={handleUpload}
                            fullWidth
                            disabled={uploading}
                        >
                            Hochladen
                        </Button>
                    )}
                </Grid>
                {uploading && (
                    <Grid item xs={12}>
                        <LinearProgress />
                    </Grid>
                )}
            </Grid>
        </Paper>
    );
};

export default UploadWad;
