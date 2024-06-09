import { useState } from 'react';
import { TextField, Button, Typography, List, ListItem, ListItemText, ListItemSecondaryAction, Checkbox, Box } from '@mui/material';
import CloudDownloadIcon from '@mui/icons-material/CloudDownload';
import { useWadPackApi } from './useWadPackApi';

const SearchDownloadPage = () => {
    const [searchQuery, setSearchQuery] = useState('');
    const [searchResults, setSearchResults] = useState([]);
    const [selectedItems, setSelectedItems] = useState([]);
    const wadPackApi = useWadPackApi();
    const WadApi = useWadApi()

    const handleSearch = async () => {
        try {
            const response = await wadPackApi.searchWads(searchQuery);
            setSearchResults(response.data);
        } catch (error) {
            console.error('Error searching:', error);
        }
    };

    const handleDownload = async () => {
        try {
            const response = await wadApi.downloadWads(selectedItems);
            const blob = await response.blob();
            const url = window.URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = 'download.zip';
            document.body.appendChild(a);
            a.click();
            a.remove();
        } catch (error) {
            console.error('Error downloading:', error);
        }
    };

    const toggleSelection = (id) => {
        setSelectedItems(prevSelectedItems =>
            prevSelectedItems.includes(id)
                ? prevSelectedItems.filter(item => item !== id)
                : [...prevSelectedItems, id]
        );
    };

    return (
        <Box p={3}>
            <Typography variant="h4" gutterBottom>Search and Download Page</Typography>
            <TextField
                label="Search"
                variant="outlined"
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                fullWidth
                margin="normal"
            />
            <Button variant="contained" onClick={handleSearch} sx={{ mr: 2 }}>Search</Button>

            <List>
                {searchResults.map(item => (
                    <ListItem key={item.id} button onClick={() => toggleSelection(item.id)}>
                        <ListItemText primary={item.name} />
                        <ListItemSecondaryAction>
                            <Checkbox checked={selectedItems.includes(item.id)} />
                        </ListItemSecondaryAction>
                    </ListItem>
                ))}
            </List>

            <Button
                variant="contained"
                onClick={handleDownload}
                disabled={selectedItems.length === 0}
                startIcon={<CloudDownloadIcon />}
            >
                Download
            </Button>
        </Box>
    );
};

export default SearchDownloadPage;
