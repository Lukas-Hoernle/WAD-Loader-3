import { useState, useEffect } from 'react';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox } from '@mui/material';
import { useWadPackApi } from './useWadPackApi';

function CreateWadPack() {
    const [wads, setWads] = useState([]);
    const [selectedWads, setSelectedWads] = useState([]);
    const wadPackApi = useWadPackApi();

    useEffect(() => {
        const fetchWads = async () => {
            try {
                const response = await wadPackApi.getWads();
                setWads(response.data);
            } catch (error) {
                console.error('Error fetching WADs:', error);
            }
        };
        fetchWads();
    }, [wadPackApi]);

    const toggleWadSelection = (wadId) => {
        setSelectedWads(prevSelectedWads =>
            prevSelectedWads.includes(wadId)
                ? prevSelectedWads.filter(id => id !== wadId)
                : [...prevSelectedWads, wadId]
        );
    };

    const handleCreateWadPack = async () => {
        try {
            const newWadPack = {
                name: "New WadPack",
                description: "Description for the new WadPack",
                wads: selectedWads.map(id => ({ id }))
            };
            await wadPackApi.createWadPack(newWadPack);
            alert('WAD Pack created successfully!');
            setSelectedWads([]);
        } catch (error) {
            console.error('Error creating WAD Pack:', error);
            alert('Failed to create WAD Pack. Please try again.');
        }
    };

    return (
        <div>
            <Typography variant="h4">Create WadPack Page</Typography>
            <Typography variant="h5">Select WADs</Typography>
            <List>
                {wads.map(wad => (
                    <ListItem key={wad.id} dense button onClick={() => toggleWadSelection(wad.id)}>
                        <ListItemText primary={wad.name} />
                        <ListItemSecondaryAction>
                            <Checkbox checked={selectedWads.includes(wad.id)} />
                        </ListItemSecondaryAction>
                    </ListItem>
                ))}
            </List>
            <Button variant="contained" onClick={handleCreateWadPack}>Create WAD Pack</Button>
        </div>
    );
}

export default CreateWadPack;
