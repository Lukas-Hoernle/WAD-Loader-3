import Button from "@mui/material/Button/Button";
import { useDownloadApi } from "../api/hooks/useDownloadApi";
import { download } from "../utils/download";

export function HomePage() {
  const downloadApi = useDownloadApi();

  const handleClick = () =>
    downloadApi
      .downloadSetupScript()
      .then((script) => download(script, "setup-script.bat"));

  return (
    <div>
      <h3>Welcome to WadLoader3</h3>
      <p>Please use the navigation bar at the top of the side.</p>
      <hr />
      <p>
        Download and execute the setup-script. The script installs a handler so
        you can start WAD-Packs from the browser.
      </p>
      <Button variant="contained" onClick={handleClick}>
        Donwload Setup-Script
      </Button>
    </div>
  );
}
