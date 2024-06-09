import { useState, useEffect } from 'react';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox } from '@mui/material';
import { useWadPackApi } from '../api/hooks/useWadPackApi';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto } from 'wadloader3-api';

function EditWadPack() {
    const [wads, setWads] = useState<WadDto[]>([]);
    const [selectedWads, setSelectedWads] = useState<WadDto[]>([]);
    const wadPackApi = useWadPackApi();
    const wadApi = useWadApi();
    useEffect(() => {
        const fetchWads = async () => {
            try {
                const response = await wadApi.getWads();
                setWads(response);
            } catch (error) {
                console.error('Fehler beim Abrufen der WADs:', error);
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

    const handleSave = async () => {
        try {
            const updatedWadPack = {
                newWadPackDto:{
                wads: selectedWads.map(id => ({ id }))}
            };
            await wadPackApi.updateWadpack(updatedWadPack);
            alert('WAD Pack successfully updated!');
        } catch (error) {
            console.error('Fehler beim Speichern des WAD Packs:', error);
            alert('Failed to save WAD Pack. Please try again.');
        }
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
