import { BrowserRouter as Router, Routes, Route, Navigate } from 'react-router-dom';
import MainLayout from './MainLayout';
import CreateWad from './CreateWad';
import CreateWadPack from './CreateWadPack';

function App() {
  return (
    <Router>
      <Routes>
        <Route path="/" element={<MainLayout />} />
        <Route path="/createwad" element={<CreateWad />} />
        <Route path="/createwadpack" element={<CreateWadPack />} />
        <Route path="*" element={<Navigate to="/" />} />
      </Routes>
    </Router>
  );
}

export default App;
