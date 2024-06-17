import { useState, useEffect } from 'react';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox, Box, Grid, Paper } from '@mui/material';
import { useWadPackApi } from '../api/hooks/useWadPackApi';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto, WadPackDto } from 'wadloader3-api';

function CreateWadPack() {
    const [wads, setWads] = useState<WadDto[]>([]);
    const [wadPacks, setWadPacks] = useState<WadPackDto[]>([]);
    const [selectedWads, setSelectedWads] = useState<WadDto[]>([]);
    const wadApi = useWadApi();
    const wadPackApi = useWadPackApi();

    useEffect(() => {
        const fetchData = async () => {
            const wadResponse = await wadApi.getWads();
            setWads(wadResponse);

            const wadPackResponse = await wadPackApi.getWadPacks();
            setWadPacks(wadPackResponse);
        };
        fetchData();
    }, [wadApi, wadPackApi]);

    const toggleWadSelection = (wad: WadDto) => {
        setSelectedWads(prevSelectedWads =>
            prevSelectedWads.includes(wad)
                ? prevSelectedWads.filter(selectedWad => selectedWad.id !== wad.id)
                : [...prevSelectedWads, wad]
        );
    };

    const addAllWads = () => {
        setSelectedWads(wads);
    };

    const removeAllWads = () => {
        setSelectedWads([]);
    };

    const handleSave = async () => {
        const newWadPack = {
            newWadPackDto: {
                name: "New WadPack",
                description: "Description for the new WadPack",
                wads: selectedWads,
            }
        };
        await wadPackApi.postWadpack(newWadPack);
        alert('WAD Pack created successfully!');
        setSelectedWads([]);
    };

    const handleRemoveWad = (wad: WadDto) => {
        setSelectedWads(prevSelectedWads => prevSelectedWads.filter(selectedWad => selectedWad.id !== wad.id));
    };

    return (
        <Box sx={{ p: 4 }}>
            <Typography variant="h4" align="center" gutterBottom>Create WadPack Page</Typography>
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
                        <Typography variant="h5" gutterBottom>Wads in Wad-Pack</Typography>
                        <List>
                            {selectedWads.map(wad => (
                                <ListItem key={wad.id} dense button onClick={() => handleRemoveWad(wad)}>
                                    <ListItemText primary={wad.name} />
                                    <ListItemSecondaryAction>
                                        <Checkbox checked />
                                    </ListItemSecondaryAction>
                                </ListItem>
                            ))}
                        </List>
                    </Paper>
                </Grid>
                <Grid item xs={12} sm={4}>
                    <Paper elevation={3} sx={{ p: 2 }}>
                        <Typography variant="h5" gutterBottom>Wad-Packs</Typography>
                        <List>
                            {wadPacks.map(wadPack => (
                                <ListItem key={wadPack.id} dense>
                                    <ListItemText primary={wadPack.name} secondary={wadPack.description} />
                                </ListItem>
                            ))}
                        </List>
                    </Paper>
                </Grid>
            </Grid>
            <Box display="flex" justifyContent="space-around" mt={4}>
                <Button variant="contained" onClick={addAllWads}>Add All Wads</Button>
                <Button variant="contained" onClick={removeAllWads}>Remove All Wads</Button>
                <Button variant="contained" onClick={handleSave}>Save</Button>
                <Button variant="contained" onClick={() => setSelectedWads([])}>Remove Wad</Button>
            </Box>
        </Box>
    );
}

export default CreateWadPack;
