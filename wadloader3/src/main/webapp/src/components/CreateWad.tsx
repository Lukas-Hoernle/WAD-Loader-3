import {
  Box,
  Button,
  Checkbox,
  Grid,
  List,
  ListItem,
  ListItemSecondaryAction,
  ListItemText,
  Paper,
  Typography,
} from "@mui/material";
import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { WadDto } from "wadloader3-api";
import { useWadApi } from "../api/hooks/useWadApi";

function CreateWad() {
  const [wads, setWads] = useState<WadDto[]>([]);
  const [selectedWads, setSelectedWads] = useState<WadDto[]>([]);
  const wadApi = useWadApi();
  const navigate = useNavigate();

  useEffect(() => {
    const fetchData = async () => {
      const wadResponse = await wadApi.getWads();
      setWads(wadResponse);
    };
    fetchData();
  }, [wadApi]);

  const toggleWadSelection = (wad: WadDto) => {
    setSelectedWads((prevSelectedWads) =>
      prevSelectedWads.includes(wad)
        ? prevSelectedWads.filter((selectedWad) => selectedWad.id !== wad.id)
        : [...prevSelectedWads, wad]
    );
  };

  const handleUploadWad = async (
    event: React.ChangeEvent<HTMLInputElement>
  ) => {
    if (event.target.files && event.target.files.length > 0) {
      const file = event.target.files[0];

      if (file.name.endsWith(".wad") || file.name.endsWith(".pk3")) {
        const formData = new FormData();
        formData.append("file", file);
        formData.append("name", file.name);
        try {
          await wadApi.postWad({
            name: file.name,
            // TODO: LUKAS add input field for
            description: "Default description",
            file: file,
          });
          const updatedWads = await wadApi.getWads();
          setWads(updatedWads);
        } catch (error) {
          console.error("Error uploading wad:", error);
        }
      } else {
        alert("Invalid file type. Only .wad and .pk3 files are allowed.");
      }
    }
  };

  return (
    <Box sx={{ p: 4 }}>
      <Typography variant="h4" align="center" gutterBottom>
        Create Wads Page
      </Typography>
      <Grid container spacing={4}>
        <Grid item xs={12} sm={6}>
          <Paper elevation={3} sx={{ p: 2 }}>
            <Typography variant="h5" gutterBottom>
              Wads
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
                    <Checkbox checked={selectedWads.includes(wad)} />
                  </ListItemSecondaryAction>
                </ListItem>
              ))}
            </List>
          </Paper>
        </Grid>
        <Grid item xs={12} sm={6}>
          <Paper elevation={3} sx={{ p: 2 }}>
            <Typography variant="h5" gutterBottom>
              Uploaded Wads
            </Typography>
            <List>
              {selectedWads.map((wad) => (
                <ListItem key={wad.id} dense>
                  <ListItemText primary={wad.name} />
                </ListItem>
              ))}
            </List>
          </Paper>
        </Grid>
      </Grid>
      <Box display="flex" justifyContent="space-around" mt={4}>
        <input
          accept=".wad,.pk3" // Accept .wad and .pk3 file types
          style={{ display: "none" }}
          id="upload-wad"
          type="file"
          onChange={handleUploadWad}
        />
        <label htmlFor="upload-wad">
          <Button variant="contained" component="span">
            Upload WAD/PK3
          </Button>
        </label>
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

export default CreateWad;
