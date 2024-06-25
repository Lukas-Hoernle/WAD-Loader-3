import { useState, useEffect } from 'react';
import { List, ListItem, ListItemText, Typography, Box, Button } from '@mui/material';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto, GetWadRequest } from 'wadloader3-api';

function WadList() {
    const [wads, setWads] = useState<WadDto[]>([]);
    const wadApi = useWadApi();

    useEffect(() => {
        const fetchWads = async () => {
            const wadResponse = await wadApi.getWads();
            setWads(wadResponse);
        };
        fetchWads();
    }, [wadApi]);

    const handleDownload = async (wadId: number, fileExtension: string) => {
        const getWadRequest: GetWadRequest = { id: wadId };
        const wad = await wadApi.getWad(getWadRequest);
        const wadBlob = new Blob([JSON.stringify(wad)], { type: 'application/octet-stream' }); 
        const url = window.URL.createObjectURL(wadBlob);
        const link = document.createElement('a');
        link.href = url;
        link.setAttribute('download', `${wad.name}.${fileExtension}`); 
        document.body.appendChild(link);
        link.click();
        link.parentNode?.removeChild(link);
    };

    return (
        <Box>
            <Typography variant="h4">Wad List</Typography>
            <List>
                {wads.map((wad) => (
                    <ListItem key={wad.id}>
                        <ListItemText primary={wad.name} secondary={wad.description} />
                        <Button variant="contained" color="primary" onClick={() => handleDownload(wad.id, 'wad')}>
                            Download as WAD
                        </Button>
                        <Button variant="contained" color="secondary" onClick={() => handleDownload(wad.id, 'pk3')}>
                            Download as PK3
                        </Button>
                    </ListItem>
                ))}
            </List>
        </Box>
    );
}

export default WadList;
