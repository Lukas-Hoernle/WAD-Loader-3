package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3.ddd1infrastructure.FsWadFileManager;
import com.luma.wadloader3.ddd1infrastructure.config.AllowedFileExtension;
import com.luma.wadloader3.ddd1infrastructure.config.WadDir;
import com.luma.wadloader3.ddd2application.config.TestConfig;
import com.luma.wadloader3.ddd2application.controller.mappers.WadMapper;
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@SpringBootTest(classes = {
        WadMapper.class,
        AllowedFileExtension.class,
        FsWadFileManager.class,
        WadDir.class,
        TestConfig.class
})
class WadControllerTest {

    @Autowired
    WadFileManager wadFileManager;
    @Autowired
    WadMapper wadMapper;
    @Autowired
    WadDir wadDir;
    WadController wadController;
    @Mock
    WadRepo wadRepo;

    @BeforeEach
    void setup() {
        wadController = new WadController(wadRepo, wadFileManager, wadMapper);
    }

    @Test
    void uploadWad() throws IOException {
        when(wadRepo.save(any())).thenAnswer(invocationOnMock -> invocationOnMock.getArgument(0));
        when(wadRepo.existsByName(any())).thenReturn(false);

        File f = new File("src/test/resources/testwad.pk3");
        MultipartFile multipartFile = new MockMultipartFile("testwad.pk3", new FileInputStream(f));

        String name = "mywad.pk3";
        String description = "random description";

        WadDto wadDto = wadController.postWad(name, description, multipartFile).getBody();

        assertNotNull(wadDto);
        assertEquals(name, wadDto.getName());
        assertEquals(description, wadDto.getDescription());

        //cleanup
        try (Stream<Path> files = Files.walk(wadDir.rootPath())) {
            files.forEach(path -> path.toFile().delete());
        }
    }

}