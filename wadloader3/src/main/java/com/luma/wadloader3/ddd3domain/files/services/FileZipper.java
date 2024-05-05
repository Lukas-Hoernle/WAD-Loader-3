package com.luma.wadloader3.ddd3domain.files.services;

import com.luma.wadloader3.ddd4abstraction.functional.Failable;

import java.nio.file.Path;
import java.util.List;

public interface FileZipper {
    Failable<Path> zipFiles(List< FileToZip> files);

    record FileToZip(Path path, String name) {
    }
}
