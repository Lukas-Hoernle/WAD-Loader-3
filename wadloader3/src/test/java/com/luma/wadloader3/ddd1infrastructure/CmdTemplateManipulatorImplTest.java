package com.luma.wadloader3.ddd1infrastructure;

import com.luma.wadloader3.ddd1infrastructure.config.WadDir;
import com.luma.wadloader3.ddd1infrastructure.zipper.FileToZipService;
import com.luma.wadloader3.ddd2application.config.TestConfig;
import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import com.luma.wadloader3.ddd3domain.files.services.TemplateArgs;
import com.luma.wadloader3.ddd3domain.wad.model.Wad;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest(classes = {
        WadDir.class,
        FileToZipService.class,
        CmdTemplateManipulatorImpl.class,
        TestConfig.class
})
class CmdTemplateManipulatorImplTest {

    @Autowired
    CmdTemplateManipulatorImpl cmdTemplateManipulator;

    @AfterEach
    void tearDown() {
        Path.of("src/test/resources/runtime-test/scripts/startwadPackName.cmd").toFile().delete();
    }

    @Test
    void fillTemplate() {
        Failable<Path> newScript = cmdTemplateManipulator.fillTemplate(
                new TemplateArgs(
                        Map.of(1, Wad.builder().name("wadName").id(1).filePath(new FilePath("/some/test/path")).build()),
                        "wadPackName"));

        assertTrue(newScript.isSuccess(), () -> newScript.getFailure().toString());

        //check file name
        assertEquals("startwadPackName.cmd",newScript.getSuccess().getFileName().toString());

        //check file content
        List<String> lines;
        try {
            lines = Files.readAllLines(newScript.getSuccess());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        assertNotNull(lines);
        assertEquals("@echo off\n" +
                "\n" +
                "if \"%GZDOOM_PATH%\"==\"\" set /p \"GZDOOM_PATH=Enter the path to gzdoom.exe: \"\n" +
                "if \"%IWAD_PATH%\"==\"\" set /p \"IWAD_PATH=Enter the path to the iwad you want to use: \"\n" +
                "\n" +
                "start /b %GZDOOM_PATH% -iwad %IWAD% -file  %~dp01\n" +
                "\n" +
                "echo \"%GZDOOM_DIR%\"", String.join("\n", lines));
    }
}