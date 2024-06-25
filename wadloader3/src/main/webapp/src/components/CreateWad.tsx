import { useState, useEffect } from "react";
import {
  Box,
  Button,
  Grid,
  List,
  ListItem,
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
  const [descriptionInput, setDescriptionInput] = useState<string>("");
  const [selectedFile, setSelectedFile] = useState<File | null>(null);
  const wadApi = useWadApi();
  const navigate = useNavigate();

  useEffect(() => {
    const fetchData = async () => {
      const wadResponse = await wadApi.getWads();
      setWads(wadResponse);
    };
    fetchData();
  }, [wadApi]);

  const handleFileChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const files = event.target.files;
    if (files && files.length > 0) {
      setSelectedFile(files[0]);
    }
  };

  const handleUpload = async () => {
    if (!selectedFile) {
      console.error("No file selected.");
      return;
    }

    const formData = new FormData();
    formData.append("file", selectedFile);
    formData.append("name", selectedFile.name);
    formData.append("description", descriptionInput);

    const postWadRequest: PostWadRequest = {
      name: formData.get("name") as string,
      description: formData.get("description") as string,
      file: selectedFile,
    };

    try {
      await wadApi.postWad(postWadRequest);
      const updatedWads = await wadApi.getWads();
      setWads(updatedWads);
      setDescriptionInput("");
      setSelectedFile(null); 
    } catch (error) {
      console.error("Error uploading wad:", error);
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
                <ListItem key={wad.id}>
                  <ListItemText primary={wad.name} />
                </ListItem>
              ))}
            </List>
          </Paper>
        </Grid>
      </Grid>
      <Box display="flex" flexDirection="column" alignItems="center" mt={4}>
        <TextField
          label="Description"
          variant="outlined"
          value={descriptionInput}
          onChange={(e) => setDescriptionInput(e.target.value)}
          sx={{ mb: 2 }}
        />
        <Box display="flex" justifyContent="space-between" alignItems="center" width="100%">
          <Box>
            <input
              accept=".wad,.pk3"
              style={{ display: "none" }}
              id="upload-wad"
              type="file"
              onChange={handleFileChange}
            />
            <label htmlFor="upload-wad">
              <Button variant="contained" component="span">
                Choose File
              </Button>
            </label>
            {selectedFile && <Typography>{selectedFile.name}</Typography>}
          </Box>
          <Button
            variant="contained"
            color="primary"
            onClick={handleUpload}
            disabled={!selectedFile || !descriptionInput.trim()}
          >
            Upload
          </Button>
          <Button
            variant="contained"
            color="primary"
            onClick={() => navigate(-1)}
            sx={{ ml: 2 }}
          >
            Back
          </Button>
        </Box>
      </Box>
    </Box>
  );
}

export default CreateWad;
