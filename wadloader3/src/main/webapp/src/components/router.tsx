import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
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
      </Routes>
    </Router>
  );
}

export default App;
