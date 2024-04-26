package com.luma.wadloader3.ddd1infrastructure;

import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import com.luma.wadloader3.ddd4abstraction.functional.Either;
import com.luma.wadloader3.ddd4abstraction.functional.ErrorMessage;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Path;

import static com.luma.wadloader3.ddd2application.config.TestConfig.TEST_WAD_SAVE_DIR;
import static org.junit.jupiter.api.Assertions.*;

class FsWadFileManagerTest {
    FsWadFileManager wadFileManager = new FsWadFileManager(Path.of(TEST_WAD_SAVE_DIR));
    File f = new File("src/test/resources/testwad.pk3");
    MultipartFile multipartFile;
    File wadFile;

    @BeforeEach
    void setup() throws IOException {
        if (wadFile != null && wadFile.exists()) wadFile.delete();
        multipartFile = new MockMultipartFile("testwad.pk3", new FileInputStream(f));
    }

    @AfterEach
    void cleanup() {
        if (wadFile != null && wadFile.exists()) wadFile.delete();
    }

    @Test
    void saveFileWorks() {
        Either<ErrorMessage, FilePath> result = wadFileManager.saveFile("WadName", multipartFile);

        assertInstanceOf(Either.Right.class, result, result.getLeft().map(ErrorMessage::error).orElse("Works"));
        File wadFile = Path.of(result.getRight().get().path()).toFile();

        assertTrue(wadFile.exists());
        assertTrue(wadFile.isFile());
        assertEquals(f.length(), wadFile.length());

        //cleanup
        assertTrue(wadFile.delete());
    }

    @Test
    void saveFileFailsOnExistingFile() {
        //first file works
        Either<ErrorMessage, FilePath> result = wadFileManager.saveFile("WadName", multipartFile);

        assertInstanceOf(Either.Right.class, result, result.getLeft().map(ErrorMessage::error).orElse("Works"));
        wadFile = Path.of(result.getRight().get().path()).toFile();

        assertTrue(wadFile.exists());
        assertTrue(wadFile.isFile());
        assertEquals(f.length(), wadFile.length());

        //second file fails
        Either<ErrorMessage, FilePath> result2 = wadFileManager.saveFile("WadName", multipartFile);
        assertInstanceOf(Either.Left.class, result2, "No Error occurred when saving same file twice");

        assertEquals(new ErrorMessage("File already exists"), result2.getLeft().get());
    }
}