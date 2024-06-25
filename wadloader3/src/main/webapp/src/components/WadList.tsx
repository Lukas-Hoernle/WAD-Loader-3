import { useState, useEffect } from 'react';
import { List, ListItem, ListItemText, Typography, Box, Button, TextField } from '@mui/material';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto, GetWadRequest, PostWadRequest } from 'wadloader3-api';

function WadList() {
    const [wads, setWads] = useState<WadDto[]>([]);
    const [searchTerm, setSearchTerm] = useState<string>('');
    const [descriptionInput, setDescriptionInput] = useState<string>('');
    const [selectedFile, setSelectedFile] = useState<File | null>(null);
    const wadApi = useWadApi();

    useEffect(() => {
        const fetchWads = async () => {
            const wadResponse = await wadApi.getWads();
            setWads(wadResponse);
        };
        fetchWads();
    }, [wadApi]);

    const filteredWads = wads.filter(wad =>
        wad.name.toLowerCase().includes(searchTerm.toLowerCase())
    );

    const handleDownload = async (wadId: number) => {
        const getWadRequest: GetWadRequest = { id: wadId };
        const wad = await wadApi.getWad(getWadRequest);
        const wadBlob = new Blob([JSON.stringify(wad)], { type: 'application/octet-stream' }); 
        const url = window.URL.createObjectURL(wadBlob);
        const link = document.createElement('a');
        link.href = url;
        link.setAttribute('download', wad.name);
        document.body.appendChild(link);
        link.click();
        link.parentNode?.removeChild(link);
    };

    const handleFileChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const files = event.target.files;
        if (files && files.length > 0) {
            setSelectedFile(files[0]);
        }
    };

    const handleUpload = async () => {
        if (!selectedFile) {
            console.error('No file selected.');
            return;
        }

        const formData = new FormData();
        formData.append('file', selectedFile);
        formData.append('name', selectedFile.name);
        formData.append('description', descriptionInput);

        const postWadRequest: PostWadRequest = {
            name: selectedFile.name,
            description: descriptionInput,
            file: selectedFile 
        };

        await wadApi.postWad(postWadRequest);

        const updatedWads = await wadApi.getWads();
        setWads(updatedWads);
        setDescriptionInput('');
        setSelectedFile(null);
    };

    return (
        <Box>
            <Typography variant="h4">Wad List</Typography>
            <TextField
                fullWidth
                label="Search Wads"
                variant="outlined"
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                sx={{ mb: 2 }}
            />
            <List>
                {filteredWads.map((wad) => (
                    <ListItem key={wad.id}>
                        <ListItemText primary={wad.name} secondary={wad.description} />
                        <Button variant="contained" color="primary" onClick={() => handleDownload(wad.id)}>
                            Download
                        </Button>
                    </ListItem>
                ))}
            </List>
            <Box mt={2}>
                <Typography variant="h5">Upload New Wad</Typography>
                <TextField
                    fullWidth
                    label="Description"
                    variant="outlined"
                    value={descriptionInput}
                    onChange={(e) => setDescriptionInput(e.target.value)}
                    sx={{ mb: 2 }}
                />
                <input
                    type="file"
                    accept=".wad"
                    onChange={handleFileChange}
                    style={{ display: 'none' }}
                    id="file-upload"
                />
                <label htmlFor="file-upload">
                    <Button variant="contained" component="span">
                        Choose File
                    </Button>
                </label>
                {selectedFile && <Typography>{selectedFile.name}</Typography>}
                <Button
                    variant="contained"
                    color="primary"
                    onClick={handleUpload}
                    disabled={!selectedFile || !descriptionInput.trim()}
                    sx={{ mt: 2 }}
                >
                    Upload
                </Button>
            </Box>
        </Box>
    );
}

export default WadList;
