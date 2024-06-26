import {
  Box,
  Button,
  Checkbox,
  Divider,
  List,
  ListItem,
  ListItemSecondaryAction,
  ListItemText,
  Paper,
  TextField,
  Typography,
} from "@mui/material";
import { useEffect, useState } from "react";
import { NewWadPackDto, WadDto, WadPackDto } from "wadloader3-api";
import { useWadApi } from "../api/hooks/useWadApi";
import { useWadPackApi } from "../api/hooks/useWadPackApi";
import { useLocalHandler } from "../hooks/useLocalHandler";

function CreateWadPack() {
  const [wads, setWads] = useState<WadDto[]>([]);
  const [wadPacks, setWadPacks] = useState<WadPackDto[]>([]);
  const [selectedWadPack, setSelectedWadPack] = useState<WadPackDto | null>(null);
  const [selectedWads, setSelectedWads] = useState<WadDto[]>([]);
  const [editingWadPack, setEditingWadPack] = useState<WadPackDto | null>(null);
  const [packName, setPackName] = useState<string>("New WadPack");
  const [packDescription, setPackDescription] = useState<string>("Description for the new WadPack");
  const [searchTerm, setSearchTerm] = useState<string>(''); // State für die Suche
  const wadApi = useWadApi();
  const wadPackApi = useWadPackApi();
  const localHandler = useLocalHandler("http://localhost:8080");

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
      prevSelectedWads.some(selectedWad => selectedWad.id === wad.id)
        ? prevSelectedWads.filter(selectedWad => selectedWad.id !== wad.id)
        : [...prevSelectedWads, wad]
    );
  };

  const handleSave = async () => {
    const newWadPack: NewWadPackDto = {
      name: packName,
      description: packDescription,
      wads: selectedWads,
    };
    if (editingWadPack) {
      await wadPackApi.updateWadpack({
        newWadPackDto: newWadPack,
        id: editingWadPack.id,
      });
    } else {
      await wadPackApi.postWadpack({ newWadPackDto: newWadPack });
    }
    const updatedWadPacks = await wadPackApi.getWadPacks();
    setWadPacks(updatedWadPacks);
  };

  const handleEditWadPack = (wadPack: WadPackDto) => {
    setEditingWadPack(wadPack);
    setPackName(wadPack.name);
    setPackDescription(wadPack.description);
    setSelectedWads(wadPack.wads);
  };

  const handleDeleteWadPack = async (wadPack: WadPackDto) => {
    await wadPackApi.deleteWadpack({ id: wadPack.id });
    const updatedWadPacks = await wadPackApi.getWadPacks();
    setWadPacks(updatedWadPacks);
    setSelectedWads([]);
    setEditingWadPack(null);
  };

  const handleRemoveWad = (wadToRemove: WadDto) => {
    setSelectedWads(prevSelectedWads =>
      prevSelectedWads.filter(wad => wad.id !== wadToRemove.id)
    );
  };

  const moveWadUp = (index: number) => {
    if (index > 0) {
      const newSelectedWads = [...selectedWads];
      [newSelectedWads[index - 1], newSelectedWads[index]] = [
        newSelectedWads[index],
        newSelectedWads[index - 1],
      ];
      setSelectedWads(newSelectedWads);
    }
  };

  const moveWadDown = (index: number) => {
    if (index < selectedWads.length - 1) {
      const newSelectedWads = [...selectedWads];
      [newSelectedWads[index + 1], newSelectedWads[index]] = [
        newSelectedWads[index],
        newSelectedWads[index + 1],
      ];
      setSelectedWads(newSelectedWads);
    }
  };

  const handleDownloadWadPack = async (wadPack: WadPackDto) => {
    try {
      localHandler(
        "wadpack",
        wadPack.id,
        wadPack.wads.map((wad) => wad.id)
      );
    } catch (error) {
      console.error("Error downloading WadPack:", error);
    }
  };

  const handleWadPackClick = (wadPack: WadPackDto) => {
    if (selectedWadPack && selectedWadPack.id === wadPack.id) {
      setSelectedWadPack(null);
    } else {
      setSelectedWadPack(wadPack);
      handleEditWadPack(wadPack);
    }
  };

  const handleStartWadPack = () => {
    if (selectedWadPack) {
      const wadIds = selectedWadPack.wads.map((wad) => wad.id);
      localHandler("startwadpack", selectedWadPack.id, wadIds as [number]);
    }
  };

  const handleCancel = () => {
    setSelectedWads([]);
    setEditingWadPack(null);
  };

  // Filterung der Wad-Packs basierend auf dem Suchbegriff
  const filteredWadPacks = wadPacks.filter(wadPack =>
    wadPack.name.toLowerCase().includes(searchTerm.toLowerCase())
  );

  return (
    <Box sx={{ p: 4 }}>
      <Typography variant="h4" align="center" gutterBottom>
        Create WadPack
      </Typography>
      <Box display="flex" flexWrap="wrap" justifyContent="space-around" mt={4}>
        <Paper elevation={3} sx={{ width: "100%", p: 2, mb: 2 }}>
          <Typography variant="h5" gutterBottom>
            Available Wads
          </Typography>
          <List>
            {wads.map(wad => (
              <ListItem key={wad.id} dense button onClick={() => toggleWadSelection(wad)}>
                <ListItemText primary={wad.name} />
                <ListItemSecondaryAction>
                <Checkbox
                    checked={selectedWads.some(selectedWad => selectedWad.id === wad.id)}
                    onChange={() => toggleWadSelection(wad)}
                  />
                </ListItemSecondaryAction>
              </ListItem>
            ))}
          </List>
        </Paper>
        <Paper elevation={3} sx={{ width: "100%", p: 2, mb: 2 }}>
          <Typography variant="h5" gutterBottom>
            Selected Wads
          </Typography>
          <Divider />
          <List>
            {selectedWads.map((wad, index) => (
              <ListItem key={wad.id} dense>
                <ListItemText primary={wad.name} />
                <ListItemSecondaryAction>
                  <Button
                    size="small"
                    variant="contained"
                    color="secondary"
                    onClick={() => handleRemoveWad(wad)}
                  >
                    Remove
                  </Button>
                  <Button
                    variant="contained"
                    onClick={() => moveWadUp(index)}
                    sx={{ ml: 1 }}
                    disabled={index === 0}
                  >
                    Up
                  </Button>
                  <Button
                    variant="contained"
                    onClick={() => moveWadDown(index)}
                    sx={{ ml: 1 }}
                    disabled={index === selectedWads.length - 1}
                  >
                    Down
                  </Button>
                </ListItemSecondaryAction>
              </ListItem>
            ))}
          </List>
        </Paper>
        <Box mt={4} sx={{ width: "100%" }}>
          <TextField
            fullWidth
            label="WadPack Name"
            variant="outlined"
            value={packName}
            onChange={(e) => setPackName(e.target.value)}
            sx={{ mb: 2 }}
          />
          <TextField
            fullWidth
            label="WadPack Description"
            variant="outlined"
            value={packDescription}
            onChange={(e) => setPackDescription(e.target.value)}
            sx={{ mb: 2 }}
          />
          <Box display="flex" justifyContent="space-between">
            <Button variant="contained" color="primary" onClick={handleSave}>
              Save WadPack
            </Button>
            <Button
              variant="contained"
              color="secondary"
              onClick={handleCancel}
            >
              Cancel
            </Button>
          </Box>
        </Box>
        <Paper elevation={3} sx={{ width: "100%", p: 2, mb: 2 }}>
          <Typography variant="h5" gutterBottom>
            Wad-Packs
          </Typography>
          <Divider />
          <TextField
            fullWidth
            label="Search Wad-Packs"
            variant="outlined"
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
            sx={{ mb: 2 }}
          />
          <List>
            {filteredWadPacks.map(wadPack => (
              <ListItem
                key={wadPack.id}
                dense
                button
                onClick={() => handleWadPackClick(wadPack)}
                selected={selectedWadPack ? selectedWadPack.id === wadPack.id : false}
              >
                <ListItemText
                  primary={wadPack.name}
                  secondary={wadPack.description}
                />
              </ListItem>
            ))}
          </List>
        </Paper>
      </Box>
      {selectedWadPack && (
        <Box mt={2}>
          <Button
            variant="contained"
            color="primary"
            onClick={() => handleEditWadPack(selectedWadPack)}
            sx={{ ml: 1 }}
          >
            Edit
          </Button>
          <Button
            variant="contained"
            color="secondary"
            onClick={() => handleDeleteWadPack(selectedWadPack)}
            sx={{ ml: 1 }}
          >
            Delete
          </Button>
          <Button
            variant="contained"
            onClick={() => handleDownloadWadPack(selectedWadPack)}
            sx={{ ml: 1 }}
          >
            Download
          </Button>
          <Button
            variant="contained"
            color="primary"
            onClick={handleStartWadPack}
            sx={{ ml: 1 }}
          >
            Start WadPack
          </Button>
        </Box>
      )}
    </Box>
  );
}

export default CreateWadPack;
