import { useState, useEffect } from 'react';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox } from '@mui/material';

function EditWadPack() {
    const [wads, setWads] = useState([]);
    const [selectedWads, setSelectedWads] = useState([]);

    useEffect(() => {
        fetchWads();//funktioniert glaub nich
    }, []);

    const fetchWads = async () => {
        try {
            const response = await fetch('/api/wads');
            const data = await response.json();
            setWads(data);
        } catch (error) {
            console.error('Fehler beim Abrufen der WADs:', error);
        }
    };

    const toggleWadSelection = (wadId: never) => {
        if (selectedWads.includes(wadId)) {
            setSelectedWads(selectedWads.filter(id => id !== wadId));
        } else {
            // @ts-ignore
            setSelectedWads([...selectedWads, wadId]);
        }
    };

    const handleSave = () => {
        console.log('Ausgewählte WADs:', selectedWads);
    };

    return (
        <div>
            <Typography variant="h4">Edit WadPack Page</Typography>
            <Typography variant="h5">WADs im Pack</Typography>
            <List>
                {selectedWads.map(id => (
                    <ListItem key={id} dense button onClick={() => toggleWadSelection(id)}>
                        <ListItemText primary={wads.find(wad => wad.id === id)?.name} />
                        <ListItemSecondaryAction>
                            <Checkbox checked />
                        </ListItemSecondaryAction>
                    </ListItem>
                ))}
            </List>
            <Typography variant="h5">Verfügbare WADs</Typography>
            <List>
                {wads.map(wad => (
                    <ListItem key={wad.id} dense button onClick={() => toggleWadSelection(wad.id)}>
                        <ListItemText primary={wad.name} />
                        {!selectedWads.includes(wad.id) && (
                            <ListItemSecondaryAction>
                                <Checkbox />
                            </ListItemSecondaryAction>
                        )}
                    </ListItem>
                ))}
            </List>
            <Button variant="contained" onClick={handleSave}>Save</Button>
        </div>
    );
}

export default EditWadPack;
