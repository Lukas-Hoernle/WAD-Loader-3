import React, { useState, useEffect } from 'react';
import {
  Button,
  Typography,
  List,
  ListItem,
  ListItemText,
  ListItemSecondaryAction,
  Checkbox,
  Box,
  Paper,
  Divider,
  TextField,
} from '@mui/material';
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
      await wadPackApi.updateWadpack({ id: editingWadPack.id, ...newWadPack });
    } else {
      await wadPackApi.postWadpack(newWadPack);
    }

    alert('WAD Pack saved successfully!');
    setSelectedWads([]);
    setEditingWadPack(null);
    setPackName('New WadPack');
    setPackDescription('Description for the new WadPack');

    const updatedWadPacks = await wadPackApi.getWadPacks();
    setWadPacks(updatedWadPacks);
  };

  const handleRemoveWad = (wad) => {
    setSelectedWads((prevSelectedWads) =>
      prevSelectedWads.filter((selectedWad) => selectedWad.id !== wad.id)
    );
  };

  const handleEditWadPack = (wadPack) => {
    setEditingWadPack(wadPack);
    setSelectedWads([...wadPack.wads]);
    setPackName(wadPack.name);
    setPackDescription(wadPack.description);
  };

  const handleDeleteWadPack = async (wadPack) => {
    await wadPackApi.deleteWadpack({ id: wadPack.id });
    const updatedWadPacks = await wadPackApi.getWadPacks();
    setWadPacks(updatedWadPacks);
  };

  const handleUploadWad = async (event) => {
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
        <Paper elevation={3} style={{ width: '30%', padding: '1em' }}>
          <Typography variant="h5" gutterBottom>Wads in Wad-Pack</Typography>
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
        <Paper elevation={3} style={{ width: '30%', padding: '1em' }}>
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
    </Box>
  );
}

export default CreateWadPack;
