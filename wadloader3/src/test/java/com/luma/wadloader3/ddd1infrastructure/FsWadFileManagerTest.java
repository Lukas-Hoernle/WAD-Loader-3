package com.luma.wadloader3.ddd1infrastructure;

import com.luma.wadloader3.ddd1infrastructure.config.AllowedFileExtension;
import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import static com.luma.wadloader3.ddd2application.config.TestConfig.TEST_WAD_SAVE_DIR;
import static org.junit.jupiter.api.Assertions.*;

class FsWadFileManagerTest {
    FsWadFileManager wadFileManager =
            new FsWadFileManager(Path.of(TEST_WAD_SAVE_DIR), new AllowedFileExtension(List.of("pk3", "wad")));
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
        Failable<FilePath> result = wadFileManager.saveFile("WadName.pk3", multipartFile);

        assertInstanceOf(Failable.Success.class, result, result.getFailure().orElse("Works"));
        File wadFile = Path.of(result.getSuccess().get().path()).toFile();

        assertTrue(wadFile.exists());
        assertTrue(wadFile.isFile());
        assertEquals(f.length(), wadFile.length());

        //cleanup
        assertTrue(wadFile.delete());
    }

    @Test
    void saveFileFailsOnExistingFile() {
        //first file works
        Failable<FilePath> result = wadFileManager.saveFile("WadName.pk3", multipartFile);

        assertInstanceOf(Failable.Success.class, result, result.getFailure().orElse("Works"));
        wadFile = Path.of(result.getSuccess().get().path()).toFile();

        assertTrue(wadFile.exists());
        assertTrue(wadFile.isFile());
        assertEquals(f.length(), wadFile.length());

        //second file fails
        Failable<FilePath> result2 = wadFileManager.saveFile("WadName.pk3", multipartFile);
        assertInstanceOf(Failable.Failure.class, result2, "No Error occurred when saving same file twice");

        assertEquals("File already exists", result2.getFailure().get());
    }
}