import { Link } from 'react-router-dom';

function HomePage() {
    return (
        <div>
            <h1>Home Page</h1>
            <Link to="/search-download">Search/Download Page</Link>
            <br />
            <Link to="/create-wadpack">Create WadPack</Link>
        </div>
    );
}

export default HomePage;
