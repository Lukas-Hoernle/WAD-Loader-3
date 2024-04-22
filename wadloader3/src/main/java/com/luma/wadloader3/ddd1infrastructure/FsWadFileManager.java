package com.luma.wadloader3.ddd1infrastructure;

import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import com.luma.wadloader3.ddd3domain.files.services.WadFileManager;
import com.luma.wadloader3.ddd4abstraction.functional.Either;
import com.luma.wadloader3.ddd4abstraction.functional.ErrorMessage;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * File manager implementation using the local users filesystem
 */
@Service
public class FsWadFileManager implements WadFileManager {

    private final Path rootDir;

    public FsWadFileManager() {
        String baseDir = System.getenv("appdata");

        if (baseDir == null) throw new RuntimeException("envar appdata is not set");

        rootDir = Paths.get(baseDir, "WadLoader3-server");
    }

    @Override
    public Either<ErrorMessage, FilePath> saveFile(String wadName, MultipartFile wadFile) {
        File file = fileByName(wadName);

        if(file.exists()) return new Either.Left<>(new ErrorMessage("File already exists"));

        try {
            wadFile.transferTo(file);
            return new Either.Right<>(new FilePath(file.getAbsolutePath()));
        } catch (IOException e) {
            return new Either.Left<>(new ErrorMessage(e.getMessage()));
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
        return "%s-%d".formatted(fileName, fileName.hashCode());
    }
}
