import { useState, useEffect } from 'react';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox, Box, Grid, Paper } from '@mui/material';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto } from 'wadloader3-api';

function CreateWad() {
    const [wads, setWads] = useState<WadDto[]>([]);
    const [selectedWads, setSelectedWads] = useState<WadDto[]>([]);
    const wadApi = useWadApi();

    useEffect(() => {
        const fetchData = async () => {
            const wadResponse = await wadApi.getWads();
            setWads(wadResponse);
        };
        fetchData();
    }, [wadApi]);

    const toggleWadSelection = (wad: WadDto) => {
        setSelectedWads(prevSelectedWads =>
            prevSelectedWads.includes(wad)
                ? prevSelectedWads.filter(selectedS ad => selectedS ad.id !== wad.id)
                : [...prevSelectedWads, wad]
        );
    };

    const addAllWads = () => {
        setSelectedWads(wads);
    };

    const removeAllWads = () => {
        setSelectedWads([]);
    };

    return (
        <Box sx={{ p: 4 }}>
            <Typography variant="h4" align="center" gutterBottom>Create Wads Page</Typography>
            <Grid container spacing={4}>
                <Grid item xs={12} sm={4}>
                    <Paper elevation={3} sx={{ p: 2 }}>
                        <Typography variant="h5" gutterBottom>Wads</Typography>
                        <List>
                            {wads.map(wad => (
                                <ListItem key={wad.id} dense button onClick={() => toggleWadSelection(wad)}>
                                    <ListItemText primary={wad.name} />
                                    <ListItemSecondaryAction>
                                        <Checkbox checked={selectedWads.includes(wad)} />
                                    </ListItemSecondaryAction>
                                </ListItem>
                            ))}
                        </List>
                    </Paper>
                </Grid>
                <Grid item xs={12} sm={4}>
                    <Paper elevation={3} sx={{ p: 2 }}>
                        <Typography variant="h5" gutterBottom>Created Wads</Typography>
                        <List>
                            {selectedWads.map(wad => (
                                <ListItem key={wad.id} dense>
                                    <ListItemText primary={wad.name} />
                                </ListItem>
                            ))}
                        </List>
                    </Paper>
                </Grid>
            </Grid>
            <Box display="flex" justifyContent="space-around" mt={4}>
                <Button variant="contained" onClick={addAllWads}>Add All Wads</Button>
                <Button variant="contained" onClick={removeAllWads}>Remove All Wads</Button>
            </Box>
        </Box>
    );
}

export default CreateWad;
