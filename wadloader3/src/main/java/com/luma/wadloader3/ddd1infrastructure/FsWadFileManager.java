package com.luma.wadloader3.ddd1infrastructure;

import com.luma.wadloader3.ddd1infrastructure.config.AllowedFileExtension;
import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import com.luma.wadloader3.ddd3domain.files.services.WadFileManager;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;
import lombok.RequiredArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * File manager implementation using the local users filesystem
 */
@RequiredArgsConstructor
public class FsWadFileManager implements WadFileManager {

    private final Path rootDir;
    private final AllowedFileExtension allowedFileExtension;
    
    @Override
    public Failable<FilePath> saveFile(String wadName, MultipartFile wadFile) {
        return fileByName(wadName).apply(file -> {
            if (file.exists()) return new Failable.Failure<>("File already exists");
            if (!rootDir.toFile().exists() && !rootDir.toFile().mkdirs())
                return new Failable.Failure<>("Root directory '%s' could not be created".formatted(rootDir));

            try {
                wadFile.transferTo(file);
                return new Failable.Success<>(new FilePath(file.getAbsolutePath()));
            } catch (IOException e) {
                return new Failable.Failure<>("Error while saving file: " + e.getMessage());
            }
        });
    }

    @Override
    public Failable<FilePath> findFileByName(String name) {
        return fileByName(name).apply(file -> file.exists()
                ? new Failable.Success<>(new FilePath(file.getAbsolutePath()))
                : new Failable.Failure<>("File not found"));
    }

    private Failable<File> fileByName(String fileName) {
        return fileName(fileName).map(s -> Paths.get(rootDir.toString(), s)
                .toFile());

    }

    private Failable<String> fileName(String fileName) {
        return allowedFileExtension.getExtensions()
                .stream()
                .filter(fileName::endsWith)
                .findAny()
                .map(ex -> "Wad-%d.%s".formatted(fileName.hashCode(), ex))
                .<Failable<String>>map(Failable.Success::new)
                .orElse(new Failable.Failure<>("File extension is not allowed"));
    }
}
