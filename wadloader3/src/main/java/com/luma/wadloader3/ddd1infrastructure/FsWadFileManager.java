package com.luma.wadloader3.ddd1infrastructure;

import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import com.luma.wadloader3.ddd3domain.files.services.WadFileManager;
import com.luma.wadloader3.ddd4abstraction.functional.Either;
import com.luma.wadloader3.ddd4abstraction.functional.ErrorMessage;
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

    @Override
    public Either<ErrorMessage, FilePath> saveFile(String wadName, MultipartFile wadFile) {
        File file = fileByName(wadName);

        if (file.exists()) return new Either.Left<>(new ErrorMessage("File already exists"));
        if (!rootDir.toFile().exists() && !rootDir.toFile().mkdirs())
            return new Either.Left<>(new ErrorMessage("Root directory '%s' could not be created".formatted(rootDir)));

        try {
            wadFile.transferTo(file);
            return new Either.Right<>(new FilePath(file.getAbsolutePath()));
        } catch (IOException e) {
            return new Either.Left<>(new ErrorMessage("Error while saving file: " + e.getMessage()));
        }
    }

    @Override
    public Either<ErrorMessage, FilePath> findFileByName(String name) {
        File wadFile = fileByName(name);
        return wadFile.exists()
                ? new Either.Right<>(new FilePath(wadFile.getAbsolutePath()))
                : new Either.Left<>(new ErrorMessage("File not found"));
    }

    private File fileByName(String fileName) {
        return Paths.get(rootDir.toString(), fileName(fileName))
                .toFile();
    }

    private String fileName(String fileName) {
        return "Wad%d-%s".formatted(fileName.hashCode(), fileName);
    }
}
