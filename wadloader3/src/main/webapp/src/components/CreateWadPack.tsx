import React, { useState, useEffect } from 'react';
import {
    Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox,
    Box, Paper, Divider, TextField
} from '@mui/material';
import { useWadPackApi } from '../api/hooks/useWadPackApi';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto, WadPackDto } from 'wadloader3-api';

function CreateWadPack() {
    const [wads, setWads] = useState<WadDto[]>([]);
    const [wadPacks, setWadPacks] = useState<WadPackDto[]>([]);
    const [selectedWads, setSelectedWads] = useState<WadDto[]>([]);
    const [editingWadPack, setEditingWadPack] = useState<WadPackDto | null>(null);
    const [packName, setPackName] = useState<string>("New WadPack");
    const [packDescription, setPackDescription] = useState<string>("Description for the new WadPack");
    const wadApi = useWadApi();
    const wadPackApi = useWadPackApi();

    useEffect(() => {
        const fetchData = async () => {
            try {
                const wadResponse = await wadApi.getWads();
                setWads(wadResponse);
                console.log("WADs geladen:", wadResponse);

                const wadPackResponse = await wadPackApi.getWadPacks();
                setWadPacks(wadPackResponse);
                console.log("Wad-Packs geladen:", wadPackResponse);
            } catch (error) {
                console.error("Fehler beim Laden von WADs oder Wad-Packs:", error);
            }
        };
        fetchData();
    }, [wadApi, wadPackApi]);

    const toggleWadSelection = (wad: WadDto) => {
        setSelectedWads(prevSelectedWads =>
            prevSelectedWads.includes(wad)
                ? prevSelectedWads.filter(selectedWad => selectedSad.id !== wad.id)
                : [...prevSelectedWads, wad]
        );
    };

    const addAllWads = () => {
        setSelectedWads(wads);
    };

    const removeAllWads = () => {
        setSelectedWads([]);
    };

    const moveWadUp = async (index: number) => {
        if (index > 0) {
            try {
                const newSelectedWads = [...selectedWads];
                [newSelectedWads[index - 1], newSelectedWads[index]] = [newSelectedWads[index], newSelectedWads[index - 1]];
                setSelectedWads(newSelectedWads);

                console.log("Vor moveWadUp API-Aufruf");
                await wadPackApi.moveWadUp(newSelectedWads[index].id); // Anpassen auf deine API-Methode
                console.log("Nach moveWadUp API-Aufruf");
            } catch (error) {
                console.error("Fehler beim Verschieben nach oben:", error);
            }
        }
    };

    const moveWadDown = async (index: number) => {
        if (index < selectedWads.length - 1) {
            try {
                const newSelectedWads = [...selectedWads];
                [newSelectedWads[index + 1], newSelectedWads[index]] = [newSelectedWads[index], newSelectedWads[index + 1]];
                setSelectedWads(newSelectedWads);

                console.log("Vor moveWadDown API-Aufruf");
                await wadPackApi.moveWadDown(newSelectedWads[index].id); // Anpassen auf deine API-Methode
                console.log("Nach moveWadDown API-Aufruf");
            } catch (error) {
                console.error("Fehler beim Verschieben nach unten:", error);
            }
        }
    };

    const handleSave = async () => {
        const newWadPack = {
            name: packName,
            description: packDescription,
            wads: selectedWads,
        };

        try {
            if (editingWadPack) {
                await wadPackApi.updateWadpack({ id: editingWadPack.id, ...newWadPack });
            } else {
                await wadPackApi.postWadpack(newWadPack);
            }

            alert('WAD Pack saved successfully!');
            setSelectedWads([]);
            setEditingWadPack(null);
            setPackName("New WadPack");
            setPackDescription("Description for the new WadPack");

            const updatedWadPacks = await wadPackApi.getWadPacks();
            setWadPacks(updatedWadPacks);
        } catch (error) {
            console.error("Fehler beim Speichern des Wad-Packs:", error);
        }
    };

    const handleRemoveWad = (wad: WadDto) => {
        setSelectedWads(prevSelectedWads => prevSelectedWads.filter(selectedWad => selectedWad.id !== wad.id));
    };

    const handleEditWadPack = (wadPack: WadPackDto) => {
        setEditingWadPack(wadPack);
        setSelectedWads(wadPack.wads);
        setPackName(wadPack.name);
        setPackDescription(wadPack.description);
    };

    const handleDeleteWadPack = async (wadPack: WadPackDto) => {
        try {
            await wadPackApi.deleteWadpack({ id: wadPack.id });
            const updatedWadPacks = await wadPackApi.getWadPacks();
            setWadPacks(updatedWadPacks);
        } catch (error) {
            console.error("Fehler beim Löschen des Wad-Packs:", error);
        }
    };

    const handleUploadWad = async (event: React.ChangeEvent<HTMLInputElement>) => {
        if (event.target.files && event.target.files.length > 0) {
            const file = event.target.files[0];
            try {
                await wadApi.uploadWad(file);
                const updatedWads = await wadApi.getWads();
                setWads(updatedWads);
            } catch (error) {
                console.error("Fehler beim Hochladen des WADs:", error);
            }
        }
    };

    return (
        <Box display="flex" flexDirection="column" alignItems="center" p={3}>
            <Typography variant="h4" gutterBottom>Create WadPack Page</Typography>
            <Box display="flex" justifyContent="space-between" width="100%" gap={2}>
                <Paper elevation={3} style={{ width: '30%', padding: '1em' }}>
                    <Typography variant="h5" gutterBottom>Wads</Typography>
                    <Divider />
                    <input
                        accept=".wad"
                        style={{ display: 'none' }}
                        id="upload-wad"
                        type="file"
                        onChange={handleUploadWad}
                    />
                    <label htmlFor="upload-wad">
                        <Button variant="contained" component="span" fullWidth>
                            Upload WAD
                        </Button>
                    </label>
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
                        {selectedWads.map((wad, index) => (
                            <ListItem key={wad.id} dense>
                                <ListItemText primary={wad.name} />
                                <ListItemSecondaryAction>
                                    <Checkbox checked />
                                    <Button onClick={() => handleRemoveWad(wad)}>Remove</Button>
                                </ListItemSecondaryAction>
                            </ListItem>
                        ))}
                    </List>
                    <Box display="flex" justifyContent="space-between" mt={2}>
                        <Button variant="contained" onClick={() => moveWadUp(selectedWads.length - 1)}>Hoch</Button>
                        <Button variant="contained" onClick={() => moveWadDown(selectedWads.length - 1)}>Runter</Button>
                    </Box>
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
        </Box>
    );
}

export default CreateWadPack;
