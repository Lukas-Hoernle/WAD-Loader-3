import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import path, {resolve} from 'node:path';
import { fileURLToPath } from "node:url";

// const serverUrl = "http://localhost:8080";

// https://vitejs.dev/config/
export default defineConfig({
  publicDir: "./src/main/webapp/public",
  build: {
    outDir: "./target/classes/static/",
    rollupOptions: {
      input: {
        main: resolve(path.dirname(fileURLToPath(import.meta.url)), "index.html")
      }
    }
  },
  plugins: [react()],
  server: {
    port: 3000,
    // proxy: {
    //   //use regex to match all backend paths
    //   "^/*/api/*": {
    //     target: serverUrl,
    //     changeOrigin: true,
    //     headers: {
    //       origin: serverUrl,
    //     },
    //     secure: false,
    //   },
    // },
  },
  resolve: {
    alias: {
      "@": path.resolve(path.dirname(fileURLToPath(import.meta.url)), "./src/main/webapp/src")
    }
  }
});
