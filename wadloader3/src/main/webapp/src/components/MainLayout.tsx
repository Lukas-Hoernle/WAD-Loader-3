import { useNavigate } from 'react-router-dom';
import { Button, Box } from '@mui/material';

function MainLayout() {
  const navigate = useNavigate();

  const navigateToCreateWad = () => {
    navigate('/createwad');
  };

  const navigateToCreateWadPack = () => {
    navigate('/createwadpack');
  };

  return (
    <Box display="flex" flexDirection="column" alignItems="center" mt={5}>
      <Button variant="contained" onClick={navigateToCreateWad}>
        Create Wad
      </Button>
      <Button variant="contained" onClick={navigateToCreateWadPack} mt={2}>
        Create WadPack
      </Button>
    </Box>
  );
}

export default MainLayout;
