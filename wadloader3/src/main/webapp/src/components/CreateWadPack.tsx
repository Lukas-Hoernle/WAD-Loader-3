import { useState, useEffect } from 'react';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox, Box, Paper, Divider, IconButton } from '@mui/material';
import { useWadPackApi } from '../api/hooks/useWadPackApi';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto, WadPackDto } from 'wadloader3-api';
import { ArrowUpward, ArrowDownward } from '@mui/icons-material';

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

    const moveWadUp = (index: number) => {
        if (index === 0) return;
        setSelectedWads(prevSelectedWads => {
            const newSelectedWads = [...prevSelectedWads];
            [newSelectedWads[index - 1], newSelectedWads[index]] = [newSelectedWads[index], newSelectedWads[index - 1]];
            return newSelectedWads;
        });
    };

    const moveWadDown = (index: number) => {
        if (index === selectedWads.length - 1) return;
        setSelectedWads(prevSelectedWads => {
            const newSelectedWads = [...prevSelectedWads];
            [newSelectedWads[index + 1], newSelectedWads[index]] = [newSelectedWads[index], newSelectedWads[index + 1]];
            return newSelectedWads;
        });
    };

    const handleUploadWad = async (event: React.ChangeEvent<HTMLInputElement>) => {
        const file = event.target.files?.[0];
        if (file) {
            const formData = new FormData();
            formData.append('file', file);
            await wadApi.uploadWad(formData);
            const updatedWads = await wadApi.getWads();
            setWads(updatedWads);
        }
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
                    <Button variant="contained" component="label">
                        Upload Wad
                        <input type="file" hidden onChange={handleUploadWad} />
                    </Button>
                </Paper>
                <Paper elevation={3} style={{ width: '30%', padding: '1em' }}>
                    <Typography variant="h5" gutterBottom>Wads in Wad-Pack</Typography>
                    <Divider />
                    <List>
                        {selectedWads.map((wad, index) => (
                            <ListItem key={wad.id} dense>
                                <ListItemText primary={wad.name} />
                                <ListItemSecondaryAction>
                                    <IconButton edge="end" onClick={() => moveWadUp(index)}>
                                        <ArrowUpward />
                                    </IconButton>
                                    <IconButton edge="end" onClick={() => moveWadDown(index)}>
                                        <ArrowDownward />
                                    </IconButton>
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
