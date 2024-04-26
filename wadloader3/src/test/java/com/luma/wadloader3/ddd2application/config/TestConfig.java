package com.luma.wadloader3.ddd2application.config;

import com.luma.wadloader3.ddd1infrastructure.FsWadFileManager;
import com.luma.wadloader3.ddd3domain.files.services.WadFileManager;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;

import java.nio.file.Paths;

@TestConfiguration
public class TestConfig {

    public final static String TEST_WAD_SAVE_DIR = "src/test/resources/generated";

    @Bean
    public WadFileManager wadFileManager() {
        // hint use other dir for testing
        String appdataDir = System.getenv("appdata");
        if (appdataDir == null) throw new RuntimeException("Envar appdata is not set");

        return new FsWadFileManager(Paths.get(TEST_WAD_SAVE_DIR, "WadLoader3-server"));
    }
}
