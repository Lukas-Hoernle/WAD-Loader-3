import { useState, useEffect } from 'react';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox, Box, Paper, Divider } from '@mui/material';
import { useWadPackApi } from '../api/hooks/useWadPackApi';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto, WadPackDto } from 'wadloader3-api';

function CreateWadPack() {
    const [wads, setWads] = useState<WadDto[]>([]);
    const [wadPacks, setWadPacks] = useState<WadPackDto[]>([]);
    const [selectedWads, setSelectedWads] = useState<WadDto[]>([]);
    const [editingWadPack, setEditingWadPack] = useState<WadPackDto | null>(null);
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
                name: editingWadPack ? editingWadPack.name : "New WadPack",
                description: editingWadPack ? editingWadPack.description : "Description for the new WadPack",
                wads: selectedWads,
            }
        };

        if (editingWadPack) {
            await wadPackApi.updateWadpack({ id: editingWadPack.id, ...newWadPack });
        } else {
            await wadPackApi.postWadpack(newWadPack);
        }

        alert('WAD Pack saved successfully!');
        setSelectedWads([]);
        setEditingWadPack(null);

        const updatedWadPacks = await wadPackApi.getWadPacks();
        setWadPacks(updatedWadPacks);
    };

    const handleRemoveWad = (wad: WadDto) => {
        setSelectedWads(prevSelectedWads => prevSelectedWads.filter(selectedWad => selectedWad.id !== wad.id));
    };

    const handleEditWadPack = (wadPack: WadPackDto) => {
        setEditingWadPack(wadPack);
        setSelectedWads(wadPack.wads);
    };

    const handleDeleteWadPack = async (wadPack: WadPackDto) => {
        await wadPackApi.deleteWadpack({ id: wadPack.id });
        const updatedWadPacks = await wadPackApi.getWadPacks();
        setWadPacks(updatedWadPacks);
    };

    return (
        <Box display="flex" flexDirection="column" alignItems="center" p={3}>
            <Typography variant="h4" gutterBottom>Create WadPack Page</Typography>
            <Box display="flex" justifyContent="space-between" width="100%" gap={2}>
                <Paper elevation={3} style={{ width: '30%', padding: '1em' }}>
                    <Typography variant="h5" gutterBottom>Wads</Typography>
                    <Divider />
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
                <Paper elevation={3} style={{ width: '30%', padding: '1em' }}>
                    <Typography variant="h5" gutterBottom>Wads in Wad-Pack</Typography>
                    <Divider />
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
                <Paper elevation={3} style={{ width: '30%', padding: '1em' }}>
                    <Typography variant="h5" gutterBottom>Wad-Packs</Typography>
                    <Divider />
                    <List>
                        {wadPacks.map(wadPack => (
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
            <Box display="flex" justifyContent="space-between" width="100%" mt={2} gap={2}>
                <Button variant="contained" onClick={addAllWads} style={{ flex: 1 }}>Add All Wads</Button>
                <Button variant="contained" onClick={removeAllWads} style={{ flex: 1 }}>Remove All Wads</Button>
                <Button variant="contained" onClick={handleSave} style={{ flex: 1 }}>Save</Button>
                <Button variant="contained" onClick={() => setSelectedWads([])} style={{ flex: 1 }}>Remove Wad</Button>
            </Box>
        </Box>
    );
}

export default CreateWadPack;
