package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3.ddd2application.config.TestConfig;
import com.luma.wadloader3.ddd3domain.files.services.WadFileManager;
import com.luma.wadloader3.ddd3domain.wad.repos.WadRepo;
import com.luma.wadloader3api.model.WadDto;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import static com.luma.wadloader3.ddd2application.config.TestConfig.TEST_WAD_SAVE_DIR;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@SpringBootTest(classes = {
        TestConfig.class
})
class WadControllerTest {

    @Autowired
    WadFileManager wadFileManager;
    WadController wadController;
    @Mock
    WadRepo wadRepo;

    @BeforeEach
    void setup() {
        wadController = new WadController(wadRepo, wadFileManager);
    }

    @Test
    void uploadWad() throws IOException {
        when(wadRepo.save(any())).thenAnswer(invocationOnMock -> invocationOnMock.getArgument(0));
        when(wadRepo.existsByName(any())).thenReturn(false);

        File f = new File("src/test/resources/testwad.pk3");
        MultipartFile multipartFile = new MockMultipartFile("testwad.pk3", new FileInputStream(f));

        String name = "mywad.pk3";
        String description = "random description";

        WadDto wadDto = wadController.wadPost(name, description, multipartFile).getBody();

        assertNotNull(wadDto);
        assertEquals(name, wadDto.getName());
        assertEquals(description, wadDto.getDescription());

        //cleanup
        try (Stream<Path> files = Files.walk(Path.of(TEST_WAD_SAVE_DIR))) {
            files.forEach(path -> path.toFile().delete());
        }
    }

}