package com.luma.wadloader3.ddd1infrastructure.zipper;

import com.luma.wadloader3.ddd3domain.files.services.FileToZipFromWadService;
import com.luma.wadloader3.ddd3domain.files.services.FileZipper;
import com.luma.wadloader3.ddd3domain.wad.model.Wad;
import org.springframework.stereotype.Service;

import java.nio.file.Path;

@Service
public class FileToZipService implements FileToZipFromWadService {
    public FileZipper.FileToZip fromWad(Wad wad) {
        return new FileZipper.FileToZip(Path.of(wad.getFilePath().path()), String.valueOf(wad.getId()));
    }
}
