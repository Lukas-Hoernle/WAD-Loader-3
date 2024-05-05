package com.luma.wadloader3.ddd1infrastructure;

import com.luma.wadloader3.ddd1infrastructure.config.AllowedFileExtension;
import com.luma.wadloader3.ddd1infrastructure.config.WadDir;
import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import com.luma.wadloader3.ddd3domain.files.services.WadFileManager;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.util.FileCopyUtils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * File manager implementation using the local users filesystem
 */
@RequiredArgsConstructor
@Service
public class FsWadFileManager implements WadFileManager {

    private final WadDir wadDir;
    private final AllowedFileExtension allowedFileExtension;
    
    @Override
    public Failable<FilePath> saveFile(String wadName, InputStream wadFile) {
        return fileByName(wadName).apply(file -> {
            if (file.exists()) return Failable.failure("File already exists");
            if (!wadDir.rootPath().toFile().exists() && !wadDir.rootPath().toFile().mkdirs())
                return Failable.failure("Root directory '%s' could not be created".formatted(wadDir.rootPath()));

            try {
                FileCopyUtils.copy(wadFile, Files.newOutputStream(file.toPath()));
                wadFile.close();
                return Failable.success(new FilePath(file.getAbsolutePath()));
            } catch (IOException e) {
                return Failable.failure("Error while saving file: " + e.getMessage());
            }
        });
    }

    @Override
    public Failable<FilePath> findFileByName(String name) {
        return fileByName(name).apply(file -> file.exists()
                ? Failable.success(new FilePath(file.getAbsolutePath()))
                : Failable.failure("File not found"));
    }

    private Failable<File> fileByName(String fileName) {
        return fileName(fileName).map(s -> Paths.get(wadDir.rootPath().toString(), s)
                .toFile());

    }

    private Failable<String> fileName(String fileName) {
        return allowedFileExtension.getExtensions()
                .stream()
                .filter(fileName::endsWith)
                .findAny()
                .map(ex -> "Wad-%d.%s".formatted(fileName.hashCode(), ex))
                .<Failable<String>>map(Failable.Success::new)
                .orElse(Failable.failure("File extension is not allowed"));
    }
}
