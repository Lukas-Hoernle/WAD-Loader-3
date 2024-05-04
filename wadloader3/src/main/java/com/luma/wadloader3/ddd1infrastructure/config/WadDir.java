package com.luma.wadloader3.ddd1infrastructure.config;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.nio.file.Path;

@ConfigurationProperties("wad.path")
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class WadDir {
    private String root;
    private String zipdir;

    public Path rootPath() {
        return Path.of(root);
    }

    public Path zipDirPath() {
        return Path.of(zipdir);
    }

    public Path packsPath() {
        return Path.of(root, "wadpacks");
    }
}
