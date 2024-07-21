import { useState, useEffect } from 'react';
import { List, ListItem, ListItemText, Typography, Box, Button, TextField } from '@mui/material';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto, GetWadRequest } from 'wadloader3-api';
import { download } from '../utils/download';

function WadList() {
    const [wads, setWads] = useState<WadDto[]>([]);
    const [searchTerm, setSearchTerm] = useState<string>('');
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
        download(wadBlob, wad.name);
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
        </Box>
    );
}

export default WadList;
