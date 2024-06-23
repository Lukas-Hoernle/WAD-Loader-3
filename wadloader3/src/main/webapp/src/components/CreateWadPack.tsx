import { useState, useEffect } from 'react';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox, Box, Paper, Divider, TextField } from '@mui/material';
import { useWadPackApi } from '../api/hooks/useWadPackApi';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto, WadPackDto } from 'wadloader3-api';

function CreateWadPack() {
    const initialWads: WadDto[] = [
        { id: 1, name: 'Wad 1' },
        { id: 2, name: 'Wad 2' },
        { id: 3, name: 'Wad 3' },
        { id: 4, name: 'Wad 4' },
    ];

    const initialSelectedWads: WadDto[] = [
        { id: 1, name: 'Wad 1' },
        { id: 3, name: 'Wad 3' },
    ];

    const [wads, setWads] = useState<WadDto[]>(initialWads);
    const [wadPacks, setWadPacks] = useState<WadPackDto[]>([]);
    const [selectedWads, setSelectedWads] = useState<WadDto[]>(initialSelectedWads);
    const [editingWadPack, setEditingWadPack] = useState<WadPackDto | null>(null);
    const [packName, setPackName] = useState<string>("New WadPack");
    const [packDescription, setPackDescription] = useState<string>("Description for the new WadPack");
    const wadApi = useWadApi();
    const wadPackApi = useWadPackApi();

    useEffect(() => {
        const fetchData = async () => {
            try {
                const wadResponse = await wadApi.getWads();
                console.log('Fetched WADs:', wadResponse);
                if (wadResponse.length === 0) {
                    setWads(initialWads);
                } else {
                    setWads(wadResponse);
                }

                const wadPackResponse = await wadPackApi.getWadPacks();
                console.log('Fetched Wad Packs:', wadPackResponse);
                setWadPacks(wadPackResponse);
            } catch (error) {
                console.error('Error fetching data:', error);
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

    const moveWadUp = (index: number) => {
        if (index > 0) {
            const newSelectedWads = [...selectedWads];
            [newSelectedWads[index - 1], newSelectedWads[index]] = [newSelectedWads[index], newSelectedWads[index - 1]];
            setSelectedWads(newSelectedWads);
        }
    };

    const moveWadDown = (index: number) => {
        if (index < selectedWads.length - 1) {
            const newSelectedWads = [...selectedWads];
            [newSelectedWads[index + 1], newSelectedWads[index]] = [newSelectedWads[index], newSelectedWads[index + 1]];
            setSelectedWads(newSelectedWads);
        }
    };

    const handleSave = async () => {
        const newWadPack = {
            name: packName,
            description: packDescription,
            wads: selectedWads,
        };

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
        await wadPackApi.deleteWadpack({ id: wadPack.id });
        const updatedWadPacks = await wadPackApi.getWadPacks();
        setWadPacks(updatedWadPacks);
    };

    const handleUploadWad = async (event: React.ChangeEvent<HTMLInputElement>) => {
        if (event.target.files && event.target.files.length > 0) {
            const file = event.target.files[0];
            await wadApi.uploadWad(file);
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
                                    <Checkbox checked={selectedWads.some(selectedWad => selectedWad.id === wad.id)} />
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
                                    <Button variant="contained" onClick={() => moveWadUp(index)}>Hoch</Button>
                                    <Button variant="contained" onClick={() => moveWadDown(index)}>Runter</Button>
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
            </Box>
            <Box display="flex" justifyContent="space-between" width="100%" mt={2} gap={2}>
                <TextField
                    label="WadPack Title"
                    variant="outlined"
                    value={packName}
                    onChange={(e) => setPackName(e.target.value)}
                    style={{ flex: 1 }}
                />
                <TextField
                    label="WadPack Description"
                    variant="outlined"
                    value={packDescription}
                    onChange={(e) => setPackDescription(e.target.value)}
                    style={{ flex: 1 }}
                />
                <Button variant="contained" onClick={handleSave} style={{ flex: 1 }}>Save</Button>
            </Box>
        </Box>
    );
}

export default CreateWadPack;
