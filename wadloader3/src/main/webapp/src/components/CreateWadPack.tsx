import { useEffect, useState } from "react";
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
import { useNavigate } from "react-router-dom";
import { NewWadPackDto, WadDto, WadPackDto } from "wadloader3-api";
import { useWadApi } from "../api/hooks/useWadApi";
import { useWadPackApi } from "../api/hooks/useWadPackApi";

function CreateWadPack() {
  const [wads, setWads] = useState<WadDto[]>([]);
  const [wadPacks, setWadPacks] = useState<WadPackDto[]>([]);
  const [selectedWadPack, setSelectedWadPack] = useState<WadPackDto | null>(null);
  const [selectedWads, setSelectedWads] = useState<WadDto[]>([]);
  const [editingWadPack, setEditingWadPack] = useState<WadPackDto | null>(null);
  const [packName, setPackName] = useState<string>("New WadPack");
  const [packDescription, setPackDescription] = useState<string>(
    "Description for the new WadPack"
  );
  const wadApi = useWadApi();
  const wadPackApi = useWadPackApi();
  const navigate = useNavigate();

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
  };

  const handleRemoveWad = (wadToRemove: WadDto) => {
    setSelectedWads((prevSelectedWads) =>
      prevSelectedWads.filter((wad) => wad.id !== wadToRemove.id)
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
      const wadPackBinary = await wadPackApi.getWadpack({ id: wadPack.id });
      const blob = new Blob([JSON.stringify(wadPackBinary)], {
        type: "application/json",
      });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.style.display = "none";
      a.href = url;
      a.download = `${wadPack.name}.json`;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
    } catch (error) {
      console.error("Error downloading WadPack:", error);
    }
  };

  const handleWadPackClick = (wadPack: WadPackDto) => {
    if (selectedWadPack && selectedWadPack.id === wadPack.id) {
      setSelectedWadPack(null);
    } else {
      setSelectedWadPack(wadPack);
      handleEditWadPack(wadPack); // Setzt die ausgewählten Wads für die Bearbeitung
    }
  };

  return (
    <Box sx={{ p: 4 }}>
      <Typography variant="h4" align="center" gutterBottom>
        Create WadPack
      </Typography>
      <Box display="flex" flexWrap="wrap" justifyContent="space-around" mt={4}>
        <Paper elevation={3} sx={{ width: { xs: "100%", md: "30%" }, p: 2, mb: 2 }}>
          <Typography variant="h5" gutterBottom>
            Available Wads
          </Typography>
          <List>
            {wads.map((wad) => (
              <ListItem
                key={wad.id}
                dense
                button
                onClick={() => toggleWadSelection(wad)}
              >
                <ListItemText primary={wad.name} />
                <ListItemSecondaryAction>
                  <Checkbox
                    checked={selectedWads.some(
                      (selectedWad) => selectedWad.id === wad.id
                    )}
                    onChange={() => toggleWadSelection(wad)}
                  />
                </ListItemSecondaryAction>
              </ListItem>
            ))}
          </List>
        </Paper>
        <Paper elevation={3} sx={{ width: { xs: "100%", md: "30%" }, p: 2, mb: 2 }}>
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
                </ListItemSecondaryAction>
                {index === 0 && (
                  <Button
                    variant="contained"
                    onClick={() => moveWadUp(index)}
                    sx={{ ml: 1 }}
                  >
                    Up
                  </Button>
                )}
                {index === selectedWads.length - 1 && (
                  <Button
                    variant="contained"
                    onClick={() => moveWadDown(index)}
                    sx={{ ml: 1 }}
                  >
                    Down
                  </Button>
                )}
              </ListItem>
            ))}
          </List>
        </Paper>
        <Paper elevation={3} sx={{ width: { xs: "100%", md: "30%" }, p: 2, mb: 2 }}>
          <Typography variant="h5" gutterBottom>
            Wad-Packs
          </Typography>
          <Divider />
          <List>
            {wadPacks.map((wadPack) => (
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
        </Box>
      )}
      <Box mt={4}>
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
          <Button
            variant="contained"
            color="primary"
            onClick={handleSave}
          >
            Save WadPack
          </Button>
          <Button
            variant="contained"
            color="secondary"
            onClick={() => navigate("/wad-list")}
          >
            Cancel
          </Button>
        </Box>
      </Box>
    </Box>
  );
}

export default CreateWadPack;
