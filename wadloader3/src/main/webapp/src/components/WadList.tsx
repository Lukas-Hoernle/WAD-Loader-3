import { useState, useEffect } from 'react';
import { List, ListItem, ListItemText, Typography, Box } from '@mui/material';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto } from 'wadloader3-api';

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

    return (
        <Box>
            <Typography variant="h4">Wad List</Typography>
            <List>
                {wads.map((wad) => (
                    <ListItem key={wad.id}>
                        <ListItemText primary={wad.name} secondary={wad.description} />
                    </ListItem>
                ))}
            </List>
        </Box>
    );
}

export default WadList;
