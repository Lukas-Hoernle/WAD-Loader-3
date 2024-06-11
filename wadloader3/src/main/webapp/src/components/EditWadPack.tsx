import { useState, useEffect } from 'react';
import { Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox } from '@mui/material';
import { useWadPackApi } from '../api/hooks/useWadPackApi';
import { useWadApi } from '../api/hooks/useWadApi';
import { WadDto, WadPackDto, UpdateWadpackRequest } from 'wadloader3-api';

function EditWadPack() {
    const [wads, setWads] = useState<WadDto[]>([]);
    const [selectedWads, setSelectedWads] = useState<WadDto[]>([]);
    const [wadPack, setWadPack] = useState<WadPackDto | null>(null);
    const wadPackApi = useWadPackApi();
    const wadApi = useWadApi();

    useEffect(() => {
        const fetchData = async () => {
            const wadResponse = await wadApi.getWads();
            setWads(wadResponse);

            const wadPackResponse = await wadPackApi.getWadpack(wadPackId);
            setWadPack(wadPackResponse);
            setSelectedWads(wadPackResponse.wads);
        };
        fetchData();
    }, [wadPackApi, wadApi]);

    const toggleWadSelection = (wad: WadDto) => {
        setSelectedWads(prevSelectedWads =>
            prevSelectedWads.includes(wad)
                ? prevSelectedWads.filter(selectedWad => selectedWad.id !== wad.id)
                : [...prevSelectedWads, wad]
        );
    };

    const handleSave = async () => {
        if (wadPack === null) {
            alert('WAD Pack ist nicht gesetzt.');
            return;
        }

        const updatedWadPack: UpdateWadpackRequest = {
            id: wadPack.id,
            newWadPackDto: {
                name: wadPack.name,
                description: wadPack.description,
                wads: selectedWads
            }
        };

        await wadPackApi.updateWadpack(updatedWadPack);
        alert('WAD Pack erfolgreich aktualisiert!');
    };

    return (
        <div>
            <Typography variant="h4">Edit WadPack Page</Typography>
            <Typography variant="h5">WADs im Pack</Typography>
            <List>
                {selectedWads.map(wad => (
                    <ListItem key={wad.id} dense button onClick={() => toggleWadSelection(wad)}>
                        <ListItemText primary={wads.find(w => w.id === wad.id)?.name} />
                        <ListItemSecondaryAction>
                            <Checkbox checked />
                        </ListItemSecondaryAction>
                    </ListItem>
                ))}
            </List>
            <Typography variant="h5">Verfügbare WADs</Typography>
            <List>
                {wads.map(wad => (
                    <ListItem key={wad.id} dense button onClick={() => toggleWadSelection(wad)}>
                        <ListItemText primary={wad.name} />
                        {!selectedWads.includes(wad) && (
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
