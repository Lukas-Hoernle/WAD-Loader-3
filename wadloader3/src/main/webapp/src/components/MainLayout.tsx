import { Outlet, Link } from "react-router-dom";
import { Box, Button } from '@mui/material';

export const MainLayout = () => {
    return (
        <Box sx={{ p: 4 }}>
            <Box display="flex" justifyContent="center" mb={2}>
                <Button component={Link} to="/" variant="contained" sx={{ mx: 1 }}>Home</Button>
                <Button component={Link} to="/create-wad" variant="contained" sx={{ mx: 1 }}>Create WAD</Button>
                <Button component={Link} to="/create-wad-pack" variant="contained" sx={{ mx: 1 }}>Create WAD Pack</Button>
            </Box>
            <Outlet />
        </Box>
    );
};
