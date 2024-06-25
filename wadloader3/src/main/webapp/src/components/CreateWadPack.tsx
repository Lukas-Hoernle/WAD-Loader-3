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
import { useNavigate } from "react-router-dom";
import { NewWadPackDto, WadDto, WadPackDto } from "wadloader3-api";
import { useWadApi } from "../api/hooks/useWadApi";
import { useWadPackApi } from "../api/hooks/useWadPackApi";

function CreateWadPack() {
  const [wads, setWads] = useState<WadDto[]>([]);
  const [wadPacks, setWadPacks] = useState<WadPackDto[]>([]);
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

  return (
    <Box sx={{ p: 4 }}>
      <Typography variant="h4" align="center" gutterBottom>
        Create WadPack
      </Typography>
      <Box display="flex" justifyContent="space-around" mt={4} flexWrap="wrap">
        <Paper elevation={3} sx={{ width: { xs: "100%", md: "30%" }, p: 2, mb: { xs: 2, md: 0 } }}>
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
        <Paper elevation={3} sx={{ width: { xs: "100%", md: "30%" }, p: 2, mb: { xs: 2, md: 0 } }}>
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
                    sx={{ mr: 1 }}
                  >
                    Remove
                  </Button>
                  <Button
                    size="small"
                    variant="contained"
                    onClick={() => moveWadUp(index)}
                    sx={{ mr: 1 }}
                  >
                    Up
                  </Button>
                  <Button
                    size="small"
                    variant="contained"
                    onClick={() => moveWadDown(index)}
                  >
                    Down
                  </Button>
                </ListItemSecondaryAction>
              </ListItem>
            ))}
          </List>
        </Paper>
        <Paper elevation={3} sx={{ width: { xs: "100%", md: "30%" }, p: 2 }}>
          <Typography variant="h5" gutterBottom>
            Wad-Packs
          </Typography>
          <Divider />
          <List>
            {wadPacks.map((wadPack) => (
              <ListItem key={wadPack.id} dense>
                <ListItemText
                  primary={wadPack.name}
                  secondary={wadPack.description}
                />
                <ListItemSecondaryAction>
                  <Box display="flex" flexDirection={{ xs: "column", md: "row" }}>
                    <Button
                      variant="contained"
                      color="primary"
                      onClick={() => handleEditWadPack(wadPack)}
                      sx={{ mr: { md: 1 }, mb: { xs: 1, md: 0 } }}
                    >
                      Edit
                    </Button>
                    <Button
                      variant="contained"
                      color="secondary"
                      onClick={() => handleDeleteWadPack(wadPack)}
                      sx={{ mr: { md: 1 }, mb: { xs: 1, md: 0 } }}
                    >
                      Delete
                    </Button>
                    <Button
                      variant="contained"
                      color="primary"
                      onClick={() => handleDownloadWadPack(wadPack)}
                    >
                      Download
                    </Button>
                  </Box>
                </ListItemSecondaryAction>
              </ListItem>
            ))}
          </List>
        </Paper>
      </Box>
      <Box width="100%" mt={3}>
        <TextField
          label="WadPack Name"
          fullWidth
          value={packName}
          onChange={(e) => setPackName(e.target.value)}
        />
      </Box>
      <Box width="100%" mt={3}>
        <TextField
          label="WadPack Description"
          fullWidth
          multiline
          rows={4}
          value={packDescription}
          onChange={(e) => setPackDescription(e.target.value)}
        />
      </Box>
      <Box width="100%" mt={3} display="flex" justifyContent="space-between">
        <Button variant="contained" color="primary" onClick={handleSave}>
          Save WadPack
        </Button>
        <Button variant="contained" color="secondary" onClick={addAllWads}>
          Add All Wads
        </Button>
        <Button variant="contained" color="secondary" onClick={removeAllWads}>
          Remove All Wads
        </Button>
      </Box>
      <Box display="flex" justifyContent="center" mt={4}>
        <Button
          variant="contained"
          color="primary"
          onClick={() => navigate(-1)}
        >
          Back
        </Button>
      </Box>
    </Box>
  );
}

export default CreateWadPack;
