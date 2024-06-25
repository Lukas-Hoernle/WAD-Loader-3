import { useEffect, useState } from "react";
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
  TextField,
} from "@mui/material";
import { useNavigate } from "react-router-dom";
import { WadDto, PostWadRequest } from "wadloader3-api";
import { useWadApi } from "../api/hooks/useWadApi";

function CreateWad() {
  const [wads, setWads] = useState<WadDto[]>([]);
  const [selectedWads, setSelectedWads] = useState<WadDto[]>([]);
  const [descriptionInput, setDescriptionInput] = useState<string>("");
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

      if (file.name.toLowerCase().endsWith(".wad") || file.name.toLowerCase().endsWith(".pk3")) {
        const formData = new FormData();
        formData.append("file", file);
        formData.append("name", file.name);
        formData.append("description", descriptionInput);

        const postWadRequest: PostWadRequest = {
          name: formData.get("name") as string,
          description: formData.get("description") as string,
          file: file,
        };

        try {
          await wadApi.postWad(postWadRequest);
          const updatedWads = await wadApi.getWads();
          setWads(updatedWads);
          setDescriptionInput(""); // Clear description input after successful upload
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
        <Grid item xs={12}>
          <Paper elevation={3} sx={{ p: 2 }}>
            <Typography variant="h5" gutterBottom>
              Uploaded Wads
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
      </Grid>
      <Box display="flex" flexDirection="column" alignItems="center" mt={4}>
        <Box>
          <input
            accept=".wad,.pk3"
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
        <Box mt={2}>
          <TextField
            label="Description"
            variant="outlined"
            value={descriptionInput}
            onChange={(e) => setDescriptionInput(e.target.value)}
          />
        </Box>
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
