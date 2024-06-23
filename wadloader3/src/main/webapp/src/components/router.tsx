import React from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import NavigationPage from './NavigationPage';
import CreateWad from './CreateWad';
import CreateWadPack from './CreateWadPack';
import WadList from './WadList';

function App() {
  return (
    <Router>
      <Routes>
        <Route path="/" element={<HomePage />} />
        <Route path="/createwad" element={<CreateWad />} />
        <Route path="/createwadpack" element={<CreateWadPack />} />
        <Route path="/wadlist" element={<WadList />} />
      </Routes>
    </Router>
  );
}

function HomePage() {
  return (
    <div>
      <h1>Auswahl text</h1>
      <p>siuuuu.</p>
    </div>
  );
}

export default App;
