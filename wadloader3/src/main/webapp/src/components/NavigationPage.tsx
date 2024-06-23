import React from 'react';
import { Link } from 'react-router-dom';
import { AppBar, Toolbar, Typography, Button, Box } from '@mui/material';

const NavigationPage = () => {
  return (
    <Box sx={{ flexGrow: 1 }}>
      <AppBar position="static">
        <Toolbar>
          <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
            WAD Loader 3
          </Typography>
          <Button component={Link} to="/createwad" color="inherit">
            Create Wad
          </Button>
          <Button component={Link} to="/createwadpack" color="inherit">
            Create WadPack
          </Button>
          <Button component={Link} to="/wadlist" color="inherit">
            Wad List
          </Button>
        </Toolbar>
      </AppBar>
    </Box>
  );
};

export default NavigationPage;
