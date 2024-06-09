import CloudDownloadIcon from '@mui/icons-material/CloudDownload';
import { Box, Button, Checkbox, List, ListItem, ListItemSecondaryAction, ListItemText, TextField, Typography } from '@mui/material';
import { useState } from 'react';
import { WadDto } from 'wadloader3-api';
import { useDownloadApi } from '../api/hooks/useDownloadApi';
import { useWadApi } from '../api/hooks/useWadApi';

const SearchDownloadPage = () => {
    const [searchQuery, setSearchQuery] = useState('');
    const [searchResults, setSearchResults] = useState<WadDto[]>([]);
    const [selectedItems, setSelectedItems] = useState<number[]>([]);
    const wadApi = useWadApi()
    const downloadApi= useDownloadApi();

    const handleSearch = async () => {
        try {
            const response = await wadApi.getWads();//gibs nich alle holen und hier searchen mit der searchQuery
            setSearchResults(response);
        } catch (error) {
            console.error('Error searching:', error);
        }
    };

    const handleDownload = async () => {
        try {
            const response = await downloadApi.downloadWad({id: selectedItems});//filtern nach selectedItems und nur die downloaden
            const url = window.URL.createObjectURL(response);
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

    const toggleSelection = (id: number) => {
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
