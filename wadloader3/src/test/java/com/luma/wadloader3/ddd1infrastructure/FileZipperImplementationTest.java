package com.luma.wadloader3.ddd1infrastructure;

import com.luma.wadloader3.ddd1infrastructure.config.WadDir;
import com.luma.wadloader3.ddd1infrastructure.zipper.FileZipperImplementation;
import com.luma.wadloader3.ddd3domain.files.services.FileZipper;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Random;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertTrue;

class FileZipperImplementationTest {

    FileZipperImplementation fileZipper;
    List<FileZipper.FileToZip> files;
    WadDir wadDir;

    @BeforeEach
    void setUp() {
        Random random = new Random();
        files = Stream.of(
                        "./src/test/resources/runtime-test/wads/file1.txt",
                        "./src/test/resources/runtime-test/wads/file2.txt")
                .map(Path::of).map(path -> new FileZipper.FileToZip(path, random.nextInt() + ""))
                .toList();
        files.stream().map(FileZipper.FileToZip::path).map(Path::toFile).forEach((f) -> {
            try {
                f.createNewFile();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
        wadDir = new WadDir("src/test/resources/runtime-test/wads", "src/test/resources/runtime-test/wads/zip", "");
        fileZipper = new FileZipperImplementation(wadDir);
    }

    @AfterEach
    void tearDown() throws IOException {
        files.stream().map(FileZipper.FileToZip::path).map(Path::toFile).forEach(File::delete);
        Files.walk(wadDir.zipDirPath()).map(Path::toFile).forEach(File::delete);
    }

    @Test
    void zipFiles() {
        Failable<Path> result = fileZipper.zipFiles(files);

        //assert file really exists
        assertTrue(result.isSuccess(), () -> result.getFailure().toString());
        assertTrue(result.getSuccess().toFile().exists());
    }
}