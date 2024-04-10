import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// https://vitejs.dev/config/
export default defineConfig({
  publicDir: "./src/main/webapp/public",
  build: {
    outDir: "./target/classes/static/",
  },
  plugins: [react()],
  server: {
    port: 3000
  }
});
