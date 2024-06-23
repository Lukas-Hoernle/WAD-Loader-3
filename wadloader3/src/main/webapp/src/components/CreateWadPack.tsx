import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox, Box, Paper, Divider, TextField } from '@mui/material';
import { useWadPackApi } from '../api/hooks/useWadPackApi';
import { useWadApi } from '../api/hooks/useWadApi';

function CreateWadPack() {
    const [wads, setWads] = useState([]);
    const [wadPacks, setWadPacks] = useState([]);
    const [selectedWads, setSelectedWads] = useState([]);
    const [editingWadPack, setEditingWadPack] = useState(null);
    const [packName, setPackName] = useState('New WadPack');
    const [packDescription, setPackDescription] = useState('Description for the new WadPack');
    const wadApi = useWadApi();
    const wadPackApi = useWadPackApi();
    const navigate = useNavigate();

    useEffect(() => {
        const fetchData = async () => {
            const wadResponse = await wadApi.getWads();
            setWads(wadResponse);
            const wadPackResponse = await wadPackApi.getWadPacks();
            setWadPacks(wadPackResponse);
        };
        fetchData();
    }, [wadApi, wadPackApi]);

    const toggleWadSelection = (wad) => {
        setSelectedWads((prevSelectedWads) =>
            prevSelectedWads.some((selectedWad) => selectedWad.id === wad.id)
                ? prevSelectedWads.filter((selectedWad) => selectedWad.id !== wad.id)
                : [...prevSelectedWads, wad]
        );
    };

    const addAllWads = () => {
        setSelectedWads([...wads]);
    };

    const removeAllWads = () => {
        setSelectedWads([]);
    };

    const moveWadUp = async (index) => {
        if (index > 0) {
            const newSelectedWads = [...selectedWads];
            [newSelectedWads[index - 1], newSelectedWads[index]] = [
                newSelectedWads[index],
                newSelectedWads[index - 1],
            ];
            setSelectedWads(newSelectedWads);
            await wadPackApi.moveWadUp(newSelectedWads[index].id);
        }
    };

    const moveWadDown = async (index) => {
        if (index < selectedWads.length - 1) {
            const newSelectedWads = [...selectedWads];
            [newSelectedWads[index + 1], newSelectedWads[index]] = [
                newSelectedWads[index],
                newSelectedWads[index + 1],
            ];
            setSelectedWads(newSelectedWads);
            await wadPackApi.moveWadDown(newSelectedWads[index].id);
        }
    };

    const handleSave = async () => {
        const newWadPack = {
            name: packName,
            description: packDescription,
            wads: selectedWads,
        };
        if (editingWadPack) {
            await wadPackApi.updateWadpack({ ...newWadPack, id: editingWadPack.id });
        } else {
            await wadPackApi.createWadPack(newWadPack);
        }
        const updatedWadPacks = await wadPackApi.getWadPacks();
        setWadPacks(updatedWadPacks);
    };

    const handleEditWadPack = (wadPack) => {
        setEditingWadPack(wadPack);
        setPackName(wadPack.name);
        setPackDescription(wadPack.description);
        setSelectedWads(wadPack.wads);
    };

    const handleDeleteWadPack = async (wadPack) => {
        await wadPackApi.deleteWadPack(wadPack.id);
        const updatedWadPacks = await wadPackApi.getWadPacks();
        setWadPacks(updatedWadPacks);
    };

    return (
        <Box sx={{ p: 4 }}>
            <Typography variant="h4" align="center" gutterBottom>Create WadPack</Typography>
            <Box display="flex" justifyContent="space-around" mt={4}>
                <Paper elevation={3} sx={{ width: '30%', p: 2 }}>
                    <Typography variant="h5" gutterBottom>Available Wads</Typography>
                    <List>
                        {wads.map((wad) => (
                            <ListItem key={wad.id} dense button onClick={() => toggleWadSelection(wad)}>
                                <ListItemText primary={wad.name} />
                                <ListItemSecondaryAction>
                                    <Checkbox checked={selectedWads.some((selectedWad) => selectedWad.id === wad.id)} />
                                </ListItemSecondaryAction>
                            </ListItem>
                        ))}
                    </List>
                </Paper>
                <Paper elevation={3} sx={{ width: '30%', p: 2 }}>
                    <Typography variant="h5" gutterBottom>Selected Wads</Typography>
                    <Divider />
                    <List>
                        {selectedWads.map((wad, index) => (
                            <ListItem key={wad.id} dense>
                                <ListItemText primary={wad.name} />
                                <ListItemSecondaryAction>
                                    <Button onClick={() => handleRemoveWad(wad)}>Remove</Button>
                                </ListItemSecondaryAction>
                            </ListItem>
                        ))}
                    </List>
                    <Box display="flex" justifyContent="space-between" mt={2}>
                        <Button variant="contained" onClick={() => moveWadUp(selectedWads.length - 1)}>Up</Button>
                        <Button variant="contained" onClick={() => moveWadDown(selectedWads.length - 1)}>Down</Button>
                    </Box>
                </Paper>
                <Paper elevation={3} sx={{ width: '30%', p: 2 }}>
                    <Typography variant="h5" gutterBottom>Wad-Packs</Typography>
                    <Divider />
                    <List>
                        {wadPacks.map((wadPack) => (
                            <ListItem key={wadPack.id} dense>
                                <ListItemText primary={wadPack.name} secondary={wadPack.description} />
                                <ListItemSecondaryAction>
                                    <Button variant="contained" color="primary" onClick={() => handleEditWadPack(wadPack)}>Edit</Button>
                                    <Button variant="contained" color="secondary" onClick={() => handleDeleteWadPack(wadPack)}>Delete</Button>
                                </ListItemSecondaryAction>
                            </ListItem>
                        ))}
                    </List>
                </Paper>
            </Box>
            <Box width="90%" mt={3}>
                <TextField
                    label="WadPack Name"
                    fullWidth
                    value={packName}
                    onChange={(e) => setPackName(e.target.value)}
                />
            </Box>
            <Box width="90%" mt={3}>
                <TextField
                    label="WadPack Description"
                    fullWidth
                    multiline
                    rows={4}
                    value={packDescription}
                    onChange={(e) => setPackDescription(e.target.value)}
                />
            </Box>
            <Box width="90%" mt={3} display="flex" justifyContent="space-between">
                <Button variant="contained" color="primary" onClick={handleSave}>Save WadPack</Button>
                <Button variant="contained" color="default" onClick={addAllWads}>Add All Wads</Button>
                <Button variant="contained" color="default" onClick={removeAllWads}>Remove All Wads</Button>
            </Box>
            <Box display="flex" justifyContent="center" mt={4}>
                <Button variant="contained" color="primary" onClick={() => navigate(-1)}>
                    Back
                </Button>
            </Box>
        </Box>
    );
}

export default CreateWadPack;
