import { AppBar, Button, Toolbar, Typography } from "@mui/material";
import { Link, Outlet } from "react-router-dom";
import { useLogout } from "../hooks/useLogout";

const NavigationPage = () => {
  const logout = useLogout();
  return (
    <>
      <AppBar>
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
          <Button component={Link} to="/listwad" color="inherit">
            Wad List
          </Button>
          <Button onClick={logout} color="inherit">
            Logout
          </Button>
        </Toolbar>
      </AppBar>
      <Outlet />
    </>
  );
};

export default NavigationPage;
