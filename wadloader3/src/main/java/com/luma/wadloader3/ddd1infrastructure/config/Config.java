package com.luma.wadloader3.ddd1infrastructure.config;

import com.luma.wadloader3.ddd1infrastructure.FsWadFileManager;
import com.luma.wadloader3.ddd3domain.files.services.WadFileManager;
import org.springframework.boot.SpringBootConfiguration;
import org.springframework.context.annotation.Bean;

import java.nio.file.Paths;

@SpringBootConfiguration
public class Config {

    @Bean
    public WadFileManager wadFileManager() {
        // hint use other dir for testing
        String appdataDir = System.getenv("appdata");
        if(appdataDir == null) throw new RuntimeException("Envar appdata is not set");

        return new FsWadFileManager(Paths.get(appdataDir, "WadLoader3-server"));
    }
}
