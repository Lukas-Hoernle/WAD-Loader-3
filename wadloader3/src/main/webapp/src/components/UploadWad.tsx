import React from 'react';
import Button from '@mui/material/Button';

function UploadWad() {
  const handleUpload = () => {
    console.log('Upload button clicked');
  };

  return (
      <div>
        <h1>Upload-Wad-Komponente</h1>
        <Button variant="contained" onClick={handleUpload}>Upload</Button>
      </div>
  );
}

export default UploadWad;
