import { useState, useEffect } from 'react';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox } from '@mui/material';

function CreateWadPack() {
    const [wads, setWads] = useState([]);
    const [selectedWads, setSelectedWads] = useState([]);

    useEffect(() => {
        fetchWads();
    }, []);

    const fetchWads = async () => {
        try {
            const response = await fetch('/api/wads');
            const data = await response.json();
            setWads(data);
        } catch (error) {
            console.error('Error fetching WADs:', error);
        }
    };

    const toggleWadSelection = (wadId: any) => {
        if (!selectedWads.includes(wadId)) {
            setSelectedWads([...selectedWads, wadId]);
        } else {
            setSelectedWads(selectedWads.filter(id => id !== wadId));
        }
};

    const handleCreateWadPack = async () => {
        try {
            const response = await fetch('/api/wadpack', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    wads: selectedWads
                })
            });
            if (response.ok) {
                alert('WAD Pack created successfully!');
                setSelectedWads([]);
            } else {
                alert('Failed to create WAD Pack.');
            }
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
